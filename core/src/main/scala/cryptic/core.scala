package cryptic

import java.nio.ByteBuffer
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.reflect.ClassTag

/** Identity type alias used to represent pure (non-effectful) values. */
type Id[A] = A
/** 32-byte SHA-256 hash represented as an immutable byte array. */
type Hash = IArray[Byte]
/** Digital signature bytes represented as an immutable byte array. */
type Signature = IArray[Byte]
/** Arbitrary metadata associated with PlainText encoding. */
type Manifest = IArray[Byte]

/** Utilities for working with Manifest metadata used during encoding. */
object Manifest:
  /** An empty manifest indicating no additional metadata. */
  val empty: Manifest = IArray.emptyByteIArray

/** A UTF-8 agnostic container for unencrypted payload bytes and their encoding manifest. */
case class PlainText(bytes: IArray[Byte], manifest: Manifest)

/** Helpers and constructors for PlainText. */
object PlainText:
  /** A PlainText containing no bytes and no manifest. */
  val empty: PlainText = PlainText(IArray.emptyByteIArray)
  /** Builds a PlainText with an empty manifest. */
  def apply(x: IArray[Byte]): PlainText = apply(x, Manifest.empty)
  /** Builds a PlainText with explicit payload bytes and manifest. */
  def apply(x: IArray[Byte], manifest: Manifest): PlainText =
    new PlainText(x, manifest)
  /** UTF-8 encodes a string into PlainText with an empty manifest. */
  def apply(x: String): PlainText = apply(x, Manifest.empty)
  /** UTF-8 encodes a string into PlainText with a manifest. */
  def apply(x: String, manifest: Manifest): PlainText =
    apply(x.getBytes().immutable, manifest)
  /** Encodes a value using its Codec and the provided manifest. */
  def encode[V: Codec](v: V, manifest: Manifest): PlainText =
    summon[Codec[V]].encode(v, manifest)
  /** Computes the SHA-256 hash of the PlainText bytes. */
  def hash(plainText: PlainText): Hash =
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(plainText.bytes.mutable).immutable // Todo handle manifest?

/** Encrypted payload bytes. The internal format may contain multiple concatenated segments. */
case class CipherText(bytes: IArray[Byte]):
  /** A mutable ByteBuffer view over the cipher bytes. */
  def buffer: ByteBuffer = ByteBuffer.wrap(bytes.mutable)
  /** Splits the bytes into the concatenated segments written via IArray.join. */
  def split: IArray[IArray[Byte]] = buffer.split
  override def equals(obj: scala.Any): Boolean = obj match
    case CipherText(other) ⇒ bytes.sameElements(other)
    case _ ⇒ false
  override def toString: String =
    s"${getClass.getCanonicalName.split('.').last}(0x${bytes.map("%02x".format(_)).mkString})"
  /** True if there are no bytes. */
  def isEmpty: Boolean = bytes.isEmpty
  /** True if there is at least one byte. */
  def nonEmpty: Boolean = bytes.nonEmpty
  /** Length of the underlying byte array. */
  def length: Int = bytes.length

/** Helpers and constructors for CipherText. */
object CipherText:
  /** A zero-length ciphertext. */
  val empty: CipherText = CipherText(IArray.emptyByteIArray)
  /** Concatenates one or more segments into a single CipherText using IArray.join. */
  def apply(array: IArray[Byte], arrays: IArray[Byte]*): CipherText =
    new CipherText(IArray.join(array, arrays*))
  /** Extractor that yields all concatenated segments written into this CipherText. */
  def unapplySeq(cipherText: CipherText): Option[Seq[IArray[Byte]]] =
    Option(cipherText.split)

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
  /**
   * Creates an instance of `Encrypt` for a given effect type, where encryption
   * produces an empty `CipherText` wrapped in the effect.
   *
   * @tparam F The effect type wrapper that determines how the encryption result
   *           is represented and executed.
   * @return An instance of `Encrypt` that always produces an empty `CipherText`
   *         wrapped in the given effect type `F`.
   */
  def empty[F[_]: Functor]: Encrypt[F] =
    _ => CipherText.empty.pure

  /** Convenience constructor for effectful encryption in any F */
  def lift[F[_]](encrypt: PlainText => F[CipherText]): Encrypt[F] =
    (plainText: PlainText) => encrypt(plainText)

//  /** Lifts a function that transforms `PlainText` to a `Try[CipherText]` into
//    * an `Encrypt` instance wrapped in the effect type `F`. The lifting process
//    * embeds the computation into the provided functor for handling effects.
//    *
//    * @param encrypt
//    *   The function that performs the transformation from `PlainText` to
//    *   `Try[CipherText]`.
//    * @param functor
//    *   Implicit evidence for the existence of a `Functor[F]`, which provides
//    *   operations for working within the effect type `F`.
//    * @return
//    *   An instance of `Encrypt[F]` that encapsulates the lifted encryption
//    *   operation.
//    */
//  def lift[F[_]: Functor](encrypt: PlainText => Try[CipherText]): Encrypt[F] =
//    (plainText: PlainText) => encrypt(plainText).lift

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
type Sign = PlainText => Array[Byte]
type Verify = Array[Byte] => Boolean

// Codec type class and helpers
trait Codec[V]:
  def encode(v: V, manifest: Manifest = Manifest.empty): PlainText
  def decode(plainText: PlainText): Try[V]

object Codec:
  trait Companion:
    extension [V: Codec](v: V)
      def encoded: PlainText = summon[Codec[V]].encode(v)
    extension [V: Codec](plainText: PlainText)
      def decoded: Try[V] = summon[Codec[V]].decode(plainText)

given Codec[Nothing]:
  def encode(v: Nothing, manifest: Manifest): PlainText =
    PlainText(IArray.emptyByteIArray, manifest)
  def decode(pt: PlainText): Try[Nothing] =
    Failure(
      new IllegalArgumentException(
        "Cannot decode Nothing"
      )
    )

// Syntax extensions
/** Enriches any value with .encrypted given Encrypt and Functor instances. */
extension [F[_]: {Functor, Encrypt}, V: Codec](value: V)
  /** Encrypts this value using the empty manifest. */
  def encrypted: Encrypted[F, V] =
    Encrypted.apply(value, Manifest.empty)
  /** Encrypts this value using the provided manifest. */
  def encrypted(manifest: Manifest): Encrypted[F, V] =
    Encrypted(value, manifest)

/** Unsafe conversions between mutable and immutable byte/char arrays. */
extension (array: Array[Byte])
  /** Immutable IArray view over a Byte array. */
  def immutable: IArray[Byte] = IArray.unsafeFromArray(array)

extension (array: IArray[Byte])
  /** Mutable Array view over an IArray[Byte]. */
  def mutable: Array[Byte] = IArray.wrapByteIArray(array).unsafeArray
extension (array: IArray[Char])
  /** Mutable Array view over an IArray[Char]. */
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

/** Concatenates one or more byte arrays with a simple length-prefixed format. */
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
