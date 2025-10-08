package cryptic

import java.nio.ByteBuffer
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.reflect.ClassTag

type Id[A] = A
type Hash = IArray[Byte]
type Signature = IArray[Byte]
type Manifest = IArray[Byte]

object Manifest:
  val empty: Manifest = IArray.emptyByteIArray

case class PlainText(bytes: IArray[Byte], manifest: Manifest)

object PlainText:
  val empty: PlainText = PlainText(IArray.emptyByteIArray)
  def apply(x: IArray[Byte]): PlainText = apply(x, Manifest.empty)
  def apply(x: IArray[Byte], manifest: Manifest): PlainText =
    new PlainText(x, manifest)
  def apply(x: String): PlainText = apply(x, Manifest.empty)
  def apply(x: String, manifest: Manifest): PlainText =
    apply(x.getBytes().immutable, manifest)
  def encode[V: Codec](v: V, manifest: Manifest): PlainText =
    summon[Codec[V]].encode(v, manifest)
  def hash(plainText: PlainText): Hash =
    import java.security.MessageDigest
    val digest = MessageDigest.getInstance("SHA-256")
    digest.digest(plainText.bytes.mutable).immutable // Todo handle manifest?

case class CipherText(bytes: IArray[Byte]):
  def buffer: ByteBuffer = ByteBuffer.wrap(bytes.mutable)
  def split: IArray[IArray[Byte]] = buffer.split
  override def equals(obj: scala.Any): Boolean = obj match
    case CipherText(other) ⇒ bytes.sameElements(other)
    case _ ⇒ false
  override def toString: String =
    s"${getClass.getCanonicalName.split('.').last}(0x${bytes.map("%02x".format(_)).mkString})"
  def isEmpty: Boolean = bytes.isEmpty
  def nonEmpty: Boolean = bytes.nonEmpty
  def length: Int = bytes.length

object CipherText:
  val empty: CipherText = CipherText(IArray.emptyByteIArray)
  def apply(array: IArray[Byte], arrays: IArray[Byte]*): CipherText =
    new CipherText(IArray.join(array, arrays*))
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
extension [F[_]: {Functor, Encrypt}, V: Codec](value: V)
  def encrypted: Encrypted[F, V] =
    Encrypted.apply(value, Manifest.empty)
  def encrypted(manifest: Manifest): Encrypted[F, V] =
    Encrypted(value, manifest)

extension (array: Array[Byte])
  def immutable: IArray[Byte] = IArray.unsafeFromArray(array)

extension (array: IArray[Byte])
  def mutable: Array[Byte] = IArray.wrapByteIArray(array).unsafeArray
extension (array: IArray[Char])
  def mutable: Array[Char] = IArray.wrapCharIArray(array).unsafeArray

extension (buffer: ByteBuffer)
  def nextBytes(): Array[Byte] =
    val length = buffer.getInt()
    val bytes =
      if length == 0 then Array.emptyByteArray else new Array[Byte](length)
    buffer.get(bytes)
    bytes
  def nextBytes(bytes: Array[Byte]): ByteBuffer =
    buffer.putInt(bytes.length)
    buffer.put(bytes)
  def split: IArray[IArray[Byte]] =
    val count = buffer.getInt()
    val arrays = Array.ofDim[IArray[Byte]](count)
    for i <- 0 until count do arrays(i) = buffer.nextBytes().immutable
    IArray.unsafeFromArray(arrays)

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
