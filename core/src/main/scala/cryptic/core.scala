package cryptic

import java.nio.ByteBuffer
import scala.annotation.targetName
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

/** Identity type alias used to represent pure (non-effectful) values. */
type Id[A] = A

/** 32-byte SHA-256 hash represented as an immutable byte array. */
type Hash = IArray[Byte]

/** Digital signature bytes represented as an immutable byte array. */
type Signature = IArray[Byte]

/** Aditional Authenticated Data associated with PlainText encoding. */
case class AAD(bytes: IArray[Byte]):
  def nonEmpty: Boolean = bytes.nonEmpty
  def int: Int = bytes.int
  def long: Long = bytes.long
  def float: Float = bytes.float
  def double: Double = bytes.double
  def string: String = new String(bytes.mutable)

  /** Executes the provided function if the current object is non-empty.
    *
    * @param f
    *   a function that takes a non-empty `IArray[Byte]` and performs an
    *   operation
    * @return
    *   Unit
    */
  def forNonEmpty[A](f: IArray[Byte] => A): Unit = if nonEmpty then f(bytes)

/** Utilities for working with AAD metadata used during encoding. */
object AAD:
  /** An empty aad indicating no additional metadata. */
  val empty: AAD = AAD(IArray.emptyByteIArray)

/** A UTF-8 agnostic container for unencrypted payload bytes and their encoding
  * aad.
  */
case class PlainText(bytes: IArray[Byte], aad: AAD)

/** Helpers and constructors for PlainText. */
object PlainText:
  /** A PlainText containing no bytes and no aad. */
  val empty: PlainText = PlainText(IArray.emptyByteIArray)

  /** Builds a PlainText with an empty aad. */
  def apply(x: IArray[Byte]): PlainText = apply(x, AAD.empty)

  /** Builds a PlainText with explicit payload bytes and aad. */
  def apply(x: IArray[Byte], aad: AAD): PlainText =
    new PlainText(x, aad)

  /** UTF-8 encodes a string into PlainText with an empty aad. */
  def apply(x: String): PlainText = apply(x, AAD.empty)

  /** UTF-8 encodes a string into PlainText with an aad. */
  def apply(x: String, aad: AAD): PlainText =
    apply(x.getBytes().immutable, aad)

  /** Encodes a value using its Codec and the provided aad. */
  def encode[V: Codec](v: V, aad: AAD): PlainText =
    summon[Codec[V]].encode(v, aad)

  /** Computes the SHA-256 hash of the PlainText bytes. */
  def hash(plainText: PlainText): Hash =
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(plainText.bytes.mutable).immutable // Todo handle aad?

sealed trait Bytes:
  val bytes: IArray[Byte]

  /** A mutable ByteBuffer view over the bytes. */
  def buffer: ByteBuffer = ByteBuffer.wrap(bytes.mutable)

  /** Splits the bytes into the concatenated segments written via IArray.join.
    */
  def split: IArray[IArray[Byte]] = buffer.split

  /** Applies a given partial function to the segments of the encrypted payload
    * bytes. If the partial function is not defined for the segments, it invokes
    * the `failed` method of the provided `Functor` instance with an
    * `IllegalArgumentException`.
    */
  def splitWith[F[_], A](
      f: PartialFunction[IArray[IArray[Byte]], F[A]]
  )(using functor: Functor[F]): F[A] =
    f.applyOrElse(
      split,
      (_: IArray[IArray[Byte]]) =>
        functor.failed(
          new IllegalArgumentException(s"Split failed, no match: $this")
        )
    )

  /** True if there are no bytes. */
  def isEmpty: Boolean = bytes.isEmpty

  /** True if there is at least one byte. */
  def nonEmpty: Boolean = bytes.nonEmpty

  /** Length of the underlying byte array. */
  def length: Int = bytes.length

  /** Shared hex formatting used by subclasses. */
  def hexBytes: String = bytes.map("%02x".format(_)).mkString

  /** Shared runtime-name formatting used by subclasses. */
  protected def runtimeName: String =
    getClass.getCanonicalName.split('.').last

/** Encrypted payload bytes. The internal format may contain multiple
  * concatenated segments.
  */
case class CipherText(bytes: IArray[Byte]) extends Bytes:
  override def toString: String = s"$runtimeName(0x$hexBytes)"

/** Helpers and constructors for CipherText. */
object CipherText:
  /** A zero-length ciphertext. */
  val empty: CipherText = CipherText(IArray.emptyByteIArray)

  /** Concatenates one or more segments into a single CipherText using
    * IArray.join.
    */
  def apply(array: IArray[Byte], arrays: IArray[Byte]*): CipherText =
    new CipherText(IArray.join(array, arrays*))

  /** Extractor that yields all concatenated segments written into this
    * CipherText.
    */
  def unapplySeq(cipherText: CipherText): Option[Seq[IArray[Byte]]] =
    Option(cipherText.split)

/** Represents a signed text structure containing a version, the original text,
 * and a cryptographic signature. This class provides functionality for
 * working with the signed data, including splitting the underlying bytes and
 * verifying the signature.
 *
 * @constructor
 * Creates a new instance of `SignedText` with the specified version, text,
 * and signature segments.
 * @param version
 * The version information encoded as bytes.
 * @param text
 * The original text data encoded as bytes.
 * @param signature
 * The cryptographic signature verifying the text's authenticity.
 */
case class SignedText(
    version: IArray[Byte],
    text: IArray[Byte],
    signature: IArray[Byte]
) extends Bytes:
  val bytes: IArray[Byte] = IArray.join(version, text, signature)
  override def toString: String = s"$runtimeName(0x$hexBytes)"

  /** Verifies this SignedText and returns the PlainText if verification
    * succeeds.
    *
    * @param verify
    *   the Verify instance to use for verification
    * @param functor
    *   the Functor instance for the effect type F
    * @return
    *   the verified PlainText wrapped in effect type F, or a failure if
    *   verification fails
    */
  def verified[F[_]](using
      verify: Verify[F],
      functor: Functor[F]
  ): F[PlainText] = verify(this)

/** Helpers and constructors for SignedText. */
object SignedText:
  /** Extractor that yields all concatenated segments written into this
    * SignedText.
    */
  def unapplySeq(signedText: SignedText): Option[Seq[IArray[Byte]]] =
    Option(signedText.split)

/** A type class for encryption, transforming plain text into encrypted cipher
  * text within a given effect type F.
  *
  * @tparam F
  *   The effect type wrapper, which determines how the encryption operation is
  *   represented and executed (e.g., synchronous, asynchronous).
  */
trait Encrypt[F[_]]:
  def apply(plainText: PlainText): F[CipherText]
object Encrypt:
  /** Creates an instance of `Encrypt` for a given effect type, where encryption
    * produces an empty `CipherText` wrapped in the effect.
    *
    * @tparam F
    *   The effect type wrapper that determines how the encryption result is
    *   represented and executed.
    * @return
    *   An instance of `Encrypt` that always produces an empty `CipherText`
    *   wrapped in the given effect type `F`.
    */
  def empty[F[_]: Functor]: Encrypt[F] =
    _ => CipherText.empty.pure

  /** Convenience constructor for effectful encryption in any F */
  def lift[F[_]](encrypt: PlainText => F[CipherText]): Encrypt[F] =
    (plainText: PlainText) => encrypt(plainText)

/** Trait that provides decryption functionality for a given effect type `F[_]`.
  *
  * This abstraction defines a decryption operation, representing the process of
  * converting encrypted data (`CipherText`) into its original plaintext form
  * (`PlainText`).
  *
  * @tparam F
  *   The effect type that wraps the result of decryption. Common effect types
  *   could include `Try`, `Option`, or data types from libraries like Cats
  *   Effect (e.g., `IO`, `Async`, etc.).
  */
trait Decrypt[F[_]]:
  def apply(cipherText: CipherText): F[PlainText]
object Decrypt:
  def empty[F[_]](using pure: PlainText => F[PlainText]): F[PlainText] = pure(
    PlainText.empty
  )
  def lift[F[_]](decrypt: CipherText => F[PlainText]): Decrypt[F] =
    (cipherText: CipherText) => decrypt(cipherText)
  def lift[F[_]: Functor](decrypt: CipherText => Try[PlainText]): Decrypt[F] =
    (cipherText: CipherText) => decrypt(cipherText).lift

// Sign/Verify function types
/** A type class for signing, transforming plain text into signed text within a
  * given effect type F.
  */
trait Sign[F[_]]:
  def apply(plainText: PlainText): F[SignedText]

/** A type class for verification, transforming signed text into plain text
  * within a given effect type F if verification succeeds.
  */
trait Verify[F[_]]:
  def apply(signedText: SignedText): F[PlainText]

/** A type class for encoding and decoding values of type `V` into and from
  * `PlainText`.
  */
trait Codec[V]:
  /** Encodes a value into PlainText with optional AAD. */
  def encode(v: V, aad: AAD = AAD.empty): PlainText

  /** Decodes PlainText into a value of type `V`. */
  def decode(plainText: PlainText): Try[V]

object Codec:
  trait Companion:
    extension [V: Codec](v: V)
      def encoded: PlainText = summon[Codec[V]].encode(v)
    extension [V: Codec](plainText: PlainText)
      def decoded: Try[V] = summon[Codec[V]].decode(plainText)

given Codec[Nothing]:
  def encode(v: Nothing, aad: AAD): PlainText =
    PlainText(IArray.emptyByteIArray, aad)
  def decode(pt: PlainText): Try[Nothing] =
    Failure(
      new IllegalArgumentException(
        "Cannot decode Nothing"
      )
    )

/** Case class Passphrase for handling cryptographic passphrases.
  *
  * @param bytes
  *   Array of bytes representing the passphrase.
  */
case class Passphrase(bytes: IArray[Byte]):
  def chars: IArray[Char] = bytes.map(_.toChar)

  override def toString: String = new String(bytes.mutable)

object Passphrase:
  def apply(password: String): Passphrase = new Passphrase(
    password.getBytes.immutable
  )

case class Salt(bytes: IArray[Byte]) extends AnyVal:
  def length: Int = bytes.length

// Syntax extensions
/** Enriches any value with .encrypted given Encrypt and Functor instances. */
extension [F[_]: {Functor, Encrypt}, V: Codec](value: V)
  /** Encrypts this value using the empty aad. */
  def encrypted: Encrypted[F, V] =
    Encrypted.apply(value, AAD.empty)

  /** Encrypts this value using the provided aad. */
  def encrypted(aad: AAD): Encrypted[F, V] =
    Encrypted(value, aad)

extension [F[_]: {Functor, Sign}, V: Codec](value: V)(using sign: Sign[F])
  /** Signs this value using the provided sign instance. */
  def signed: Signed[F, V] = Signed(sign(PlainText.encode(value, AAD.empty)))

/** Unsafe conversions between mutable and immutable byte/char arrays. */
extension (array: Array[Byte])
  /** Immutable IArray view over a Byte array. */
  @targetName("toImmutableArray")
  def immutable: IArray[Byte] = IArray.unsafeFromArray(array)

  /** Returns an instance of the `AAD` type created from the provided `array`.
    *
    * @return
    *   an instance of `AAD` initialized using the given array.
    */
  @targetName("toAAD")
  def aad: AAD = AAD(IArray.unsafeFromArray(array))

extension (str: String)
  /** Returns the `AAD` instance created from the `str` converted to bytes.
    *
    * @return
    *   an instance of `AAD` derived from the byte array representation of
    *   `str`.
    */
  @targetName("stringToAAD")
  def aad: AAD = str.getBytes.aad
  @targetName("stringToBytes")
  def bytes: IArray[Byte] = str.getBytes.immutable
  def fromResource: String = Source.fromResource(str).slurp

extension (array: IArray[Byte])
  /** Mutable Array view over an IArray[Byte]. */
  @targetName("toMutableArray")
  def mutable: Array[Byte] = IArray.wrapByteIArray(array).unsafeArray
  def byteBuffer: ByteBuffer = ByteBuffer.wrap(array.mutable)
  def split: IArray[IArray[Byte]] = byteBuffer.split
  def string: String = new String(mutable)
  def int: Int = byteBuffer.getInt
  def long: Long = byteBuffer.getLong
  def float: Float = byteBuffer.getFloat
  def double: Double = byteBuffer.getDouble

extension (array: IArray[Char])
  /** Mutable Array view over an IArray[Char]. */
  @targetName("toMutableCharArray")
  def mutable: Array[Char] = IArray.wrapCharIArray(array).unsafeArray

/** Binary encoding helpers for length-prefixed concatenation of byte arrays. */
extension (buffer: ByteBuffer)
  /** Reads a length-prefixed byte array from this buffer. */
  def nextBytes(): Array[Byte] =
    val length = buffer.getInt()
    val bytes =
      if length == 0 then Array.emptyByteArray else new Array[Byte](length)
    buffer.get(bytes)
    bytes

  /** Writes a length-prefixed byte array into this buffer. */
  def nextBytes(bytes: Array[Byte]): ByteBuffer =
    buffer.putInt(bytes.length)
    buffer.put(bytes)

  /** Reads N length-prefixed arrays written by IArray.join. */
  def split: IArray[IArray[Byte]] =
    val count = buffer.getInt()
    val arrays = Array.ofDim[IArray[Byte]](count)
    for i <- 0 until count do arrays(i) = buffer.nextBytes().immutable
    IArray.unsafeFromArray(arrays)

/** Concatenates one or more byte arrays with a simple length-prefixed format.
  */
extension (iarrayObject: IArray.type)
  def join(array: IArray[Byte], arrays: IArray[Byte]*): IArray[Byte] =
    val count = 1 + arrays.length
    val buffer =
      ByteBuffer.allocate(4 + count * 4 + arrays.foldLeft(array.length):
        case (length, bytes) => length + bytes.length)
    buffer.putInt(count)
    buffer.nextBytes(array.mutable)
    arrays.foldLeft(buffer):
      case (buffer, bytes) => buffer.nextBytes(bytes.mutable)
    buffer.array().immutable

extension (i: Int)
  def bytes: IArray[Byte] =
    val buffer = ByteBuffer.allocate(4)
    buffer.putInt(i)
    buffer.array().immutable
  def aad: AAD = AAD(bytes)

extension (l: Long)
  def bytes: IArray[Byte] =
    val buffer = ByteBuffer.allocate(8)
    buffer.putLong(l)
    buffer.array().immutable
  def aad: AAD = AAD(bytes)

extension (f: Float)
  def bytes: IArray[Byte] =
    val buffer = ByteBuffer.allocate(4)
    buffer.putFloat(f)
    buffer.array().immutable
  def aad: AAD = AAD(bytes)

extension (d: Double)
  def bytes: IArray[Byte] =
    val buffer = ByteBuffer.allocate(8)
    buffer.putDouble(d)
    buffer.array().immutable
  def aad: AAD = AAD(bytes)

extension (source: Source)
  def using[A](f: Source => A): A =
    try
      f(source)
    finally
      source.close()
  def slurp: String = source.using(_.mkString)
