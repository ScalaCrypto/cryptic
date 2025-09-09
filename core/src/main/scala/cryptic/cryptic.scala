package cryptic

import cryptic.Cryptic.Operation

import java.nio.ByteBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.reflect.ClassTag

type Hash = IArray[Byte]
type Signature = IArray[Byte]
type Manifest = IArray[Byte]

object Manifest:
  val empty: Manifest = IArray.emptyByteIArray

case class PlainText(bytes: IArray[Byte], manifest: Manifest):
  // override def toString: String = "\uD83D\uDD12"
  override def equals(obj: Any): Boolean = obj match
    case other: PlainText =>
      manifest.sameElements(other.manifest) && bytes.sameElements(other.bytes)
    case _ => false
  override def hashCode: Int =
    java.util.Arrays.hashCode(
      bytes.mutable
    ) * 31 + java.util.Arrays.hashCode(
      manifest.mutable
    )

object PlainText:
  val empty: PlainText = PlainText(IArray.emptyByteIArray)
  def apply(x: IArray[Byte]): PlainText = apply(x, Manifest.empty)
  def apply(x: IArray[Byte], manifest: Manifest): PlainText =
    new PlainText(x, manifest)
  def apply(x: String): PlainText = apply(x, Manifest.empty)
  def apply(x: String, manifest: Manifest): PlainText =
    apply(x.getBytes().immutable, manifest)
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

object CipherText:
  val Empty: CipherText = CipherText(IArray.emptyByteIArray)
  def apply(array: IArray[Byte], arrays: IArray[Byte]*): CipherText =
    new CipherText(IArray.join(array, arrays*))
  def unapplySeq(cipherText: CipherText): Option[Seq[IArray[Byte]]] =
    Option(cipherText.split)

type Encrypt = PlainText => Future[CipherText]
object Encrypt:
  val Empty: Encrypt = _ ⇒ Future.successful(CipherText.Empty)

type Decrypt = CipherText => Future[PlainText]
object Decrypt:
  val Empty: Decrypt = _ ⇒ Future.successful(PlainText.empty)

type Sign = PlainText => Array[Byte]
type Verify = Array[Byte] => Boolean

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

extension [V: Codec](value: V)
  def encrypted(using
      encrypt: Encrypt,
      ec: ExecutionContext
  ): Future[Encrypted[V]] =
    Encrypted(value, Manifest.empty)
  def encrypted(
      manifest: Manifest
  )(using encrypt: Encrypt, ec: ExecutionContext): Future[Encrypted[V]] =
    Encrypted(value, manifest)
  def encrypted[U](
      f: Encrypted[V] => U
  )(using encrypt: Encrypt, ec: ExecutionContext): Future[U] =
    value.encrypted.map(f)
  def encrypted[U](manifest: Manifest)(
      f: Encrypted[V] => U
  )(using encrypt: Encrypt, ec: ExecutionContext): Future[U] =
    value.encrypted.map(f)

extension [V: Codec](futureEnc: Future[Encrypted[V]])
  def exists(
      p: V => Boolean
  )(using decrypt: Decrypt, ec: ExecutionContext): Future[Boolean] =
    futureEnc.flatMap(_.exists(p))

  def forall(
      p: V => Boolean
  )(using decrypt: Decrypt, ec: ExecutionContext): Future[Boolean] =
    futureEnc.flatMap(_.forall(p))

  def foreach(
      f: V => Unit
  )(using decrypt: Decrypt, ec: ExecutionContext): Unit =
    futureEnc.foreach(_.foreach(f))

  def map[U: Codec](
      f: V => U
  )(using ec: ExecutionContext): Future[Operation[U]] =
    futureEnc.map(_.map(f))

  def flatMap[U: Codec](f: V => Cryptic[U])(using
      ec: ExecutionContext
  ): Future[Operation[U]] =
    futureEnc.map(_.flatMap(f))
    
  def flatMapF[U: Codec](f: V => Future[Cryptic[U]])(using
      ec: ExecutionContext
  ): Future[Operation[U]] =
    futureEnc.map(_.flatMapF(f))

  def filter(p: V => Boolean)(using
      ec: ExecutionContext
  ): Future[Operation[V]] = futureEnc.map(_.filter(p))

  def collect[U: Codec](
      pf: PartialFunction[V, U]
  )(using ec: ExecutionContext): Future[Operation[U]] =
    futureEnc.map(_.collect(pf))

  def orElse[U >: V: Codec](other: => Cryptic[U])(using
      ec: ExecutionContext
  ): Future[Operation[U]] =
    futureEnc.map(_.orElse(other))

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

sealed abstract class Cryptic[V: Codec]:
  import Cryptic.*

  /** Decrypts the data using the provided given decryption mechanism.
    *
    * @param decrypt
    *   The given decryption logic that will be used to decrypt the data.
    * @return
    *   Returns a Try instance containing the decrypted value of type V if
    *   successful, or a Failure if the decryption fails.
    */
  def decrypted(using decrypt: Decrypt, ec: ExecutionContext): Future[V]

  /** Applies a transformation to the decrypted value produced by this
    * operation.
    *
    * @param f
    *   The function to transform the decrypted value of type `V` into a new
    *   value of type `U`.
    * @return
    *   A `Future[U]` representing the result of applying the function `f` to
    *   the decrypted value.
    */
  def decrypted[U](
      f: V => U
  )(using decrypt: Decrypt, ec: ExecutionContext): Future[U] = decrypted.map(f)

  /** Attempts to retrieve the decrypted value. If decryption fails or if the
    * value is not present, it returns the provided default value.
    *
    * @param default
    *   the value to return if decryption fails or if the value is not present
    * @param decrypt
    *   a given decryption context
    * @return
    *   the decrypted value if successful, otherwise the default value
    */
  @inline final def decryptedOrElse[W >: V](default: => W)(using
      decrypt: Decrypt,
      executionContext: ExecutionContext
  ): Future[W] = decrypted.recover(_ => default)

  /** Transforms the current operation by applying a function to its result.
    *
    * @param f
    *   The function to apply to the result of this operation.
    * @tparam W
    *   The result type of the function `f` after applying it to the original
    *   result type `V`.
    * @return
    *   A new operation representing the transformation of the original
    *   operation.
    */
  @inline final def map[W: Codec](f: V => W): Operation[W] =
    Mapped(this, f)

  /** Transforms this `Operation[V]` into an `Operation[W]` by applying the
    * given function `f`. The function `f` maps a value of type `V` to a
    * `Cryptic[W]`, and the resulting `Operation` represents the composition of
    * this transformation with the original operation.
    *
    * @param f
    *   a function that takes a value of type `V` and produces a `Cryptic[W]`
    * @return
    *   an `Operation[W]` resulting from applying the function `f` to each value
    *   of type `V` produced by this `Operation`
    */
  @inline final def flatMap[W: Codec](f: V => Cryptic[W]): Operation[W] =
    FlatMapped(this, f)

  @inline final def flatMapF[W: Codec](
      f: V => Future[Cryptic[W]]
  ): Operation[W] = FlatMappedF(this, f)

  /*
  /** Returns a $cryptic which will unwrap one level of cryptic nesting once run.
   *
   * @param  ev    evidence that this $cryptic's type parameter `V` is in fact a `Cryptic[W]`.
   */
  @inline final def flatten[W : Codec](using ev: V <:< Cryptic[W]): Operation[W] = Flattened(this)
   */

  /** Filters the elements based on the provided predicate function.
    *
    * @param p
    *   the predicate function used to test elements.
    * @return
    *   a new Operation containing elements that satisfy the predicate.
    */
  @inline final def filter(p: V => Boolean): Operation[V] = Filtered(this, p)

  /** Filters out elements of a collection that satisfy a given predicate.
    *
    * @param p
    *   the predicate function used to test elements, excluding those that
    *   return true
    * @return
    *   a new collection without the elements that satisfy the predicate
    */
  @inline final def filterNot(p: V => Boolean): Filtered[V] =
    Filtered(this, v => !p(v))

  /** Applies a partial function to the elements of the collection and returns a
    * new collection containing the results.
    *
    * @param pf
    *   the partial function to be applied to each element
    * @return
    *   a new collection containing the results of applying the partial function
    */
  @inline final def collect[W: Codec](
      pf: PartialFunction[V, W]
  ): Operation[W] = Collected(this, pf)

  /** Provides an alternative operation to perform if the current operation
    * fails.
    *
    * @param alternative
    *   The alternative Cryptic operation to execute if the current one fails.
    * @return
    *   A new Operation instance representing the original or the alternative
    *   operation.
    */
  @inline final def orElse[W >: V: Codec](
      alternative: => Cryptic[W]
  ): Operation[W] =
    new OrElsed(this, alternative)
object Cryptic:
  export cryptic.{
    encrypted,
    Encrypted,
    Encrypt,
    Decrypt,
    Codec,
    PlainText,
    CipherText
  }
  import Encrypted.*
  sealed abstract class Operation[V: Codec] extends Cryptic[V]:
    def run(using
        encrypt: Encrypt,
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[Encrypted[V]]
  sealed abstract class BinaryOperation[V: Codec, W: Codec]
      extends Operation[W]:
    override def run(using
        encrypt: Encrypt,
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[Encrypted[W]] =
      decrypted.flatMap(_.encrypted)
  final case class Mapped[V: Codec, W: Codec](
      src: Cryptic[V],
      f: V => W
  ) extends BinaryOperation[V, W]:
    override def decrypted(using
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[W] =
      src.decrypted.map(f)
  final case class FlatMapped[V: Codec, W: Codec](
      src: Cryptic[V],
      f: V => Cryptic[W]
  ) extends BinaryOperation[V, W]:
    override def decrypted(using
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[W] =
      src.decrypted.flatMap[W](v => f(v).decrypted)
  final case class FlatMappedF[V: Codec, W: Codec](
      src: Cryptic[V],
      f: V => Future[Cryptic[W]]
  ) extends BinaryOperation[V, W]:
    override def decrypted(using
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[W] =
      src.decrypted.flatMap[W](v => f(v).flatMap(_.decrypted))
  /*
  final case class Flattened[V : Codec, W : Codec](src: Cryptic[V])(using ev: V <:< Cryptic[W])
      extends Operation[V]:
    override def run(using encrypt: Encrypt, decrypt: Decrypt): Try[Encrypted[V]] =
      src.decrypted.map[Encrypted[V]](ev.apply)
    override def decrypted(using decrypt: Decrypt): Try[W] =
      src.decrypted.flatMap(v => ev(v).decrypted)
   */
  final case class Filtered[V: Codec](src: Cryptic[V], pred: V => Boolean)
      extends Operation[V]:
    override def run(using
        encrypt: Encrypt,
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[Encrypted[V]] = src.decrypted.flatMap[Encrypted[V]]: v =>
      if pred(v) then v.encrypted else Future.successful(empty[V])
    override def decrypted(using
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[V] =
      src.decrypted.flatMap: v =>
        if pred(v) then Future.successful(v)
        else
          Future.failed(
            new UnsupportedOperationException(
              "decrypted called on filtered empty"
            )
          )
  final case class Collected[V: Codec, W: Codec](
      src: Cryptic[V],
      pf: PartialFunction[V, W]
  ) extends BinaryOperation[V, W]:
    override def run(using
        encrypt: Encrypt,
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[Encrypted[W]] =
      src.decrypted.flatMap:
        case v if pf.isDefinedAt(v) => pf(v).encrypted
        case _                      => Future.successful(empty[W])
    override def decrypted(using
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[W] =
      src.decrypted.flatMap:
        case v if pf.isDefinedAt(v) => Future.successful(pf(v))
        case _ =>
          Future.failed(
            new UnsupportedOperationException(
              s"decrypted called on collected empty"
            )
          )
  final class OrElsed[V: Codec, W >: V: Codec](
      src: Cryptic[V],
      alternative: => Cryptic[W]
  ) extends BinaryOperation[V, W]:
    override def decrypted(using
        decrypt: Decrypt,
        ec: ExecutionContext
    ): Future[W] =
      src.decrypted.recoverWith(_ => alternative.decrypted)

/** Represents an encrypted value of type `V`.
  *
  * @param cipherText
  *   The encrypted data as `CipherText`.
  * @tparam V
  *   The type of the value being encrypted, requiring a given `Codec` instance.
  */
case class Encrypted[V: Codec](cipherText: CipherText) extends Cryptic[V]:

  /** Returns the byte array representation of the cipher text.
    *
    * @return
    *   the byte array of the cipher text
    */
  def bytes: IArray[Byte] = cipherText.bytes

  /** Checks if the option is non-empty (defined).
    *
    * @return
    *   true if the option is defined, false otherwise.
    */
  @inline final def nonEmpty: Boolean = isDefined

  /** Checks if the value is defined.
    *
    * @return
    *   true if the value is defined, false otherwise
    */
  @inline final def isDefined: Boolean = !isEmpty

  /**
   * Checks if the decrypted value is equivalent to the given value.
   * If the encrypted value is empty, it returns `true`.
   *
   * @param value
   * The value of type `V` to compare with the decrypted value.
   * @param decrypt
   * An implicit `Decrypt` instance used for decryption.
   * @param executionContext
   * An implicit `ExecutionContext` for asynchronous processing.
   * @return
   * A `Future[Boolean]` indicating whether the decrypted value equals the given value
   * (`true`), or if the collection is empty (`true`), or if it does not match (`false`).
   */
  @inline final def contains(value: V)(using
      decrypt: Decrypt,
      executionContext: ExecutionContext
  ): Future[Boolean] =
    if isEmpty then Future.successful(true)
    else decrypted.map(_ == value)

  /**
   * Checks if there exists an element within the decrypted collection that satisfies the given predicate.
   * If the collection is empty, the result is `false`.
   *
   * @param p
   * A predicate function to test each element of type `V`.
   * @param decrypt
   * An implicit `Decrypt` instance used for decryption.
   * @param ec
   * An implicit `ExecutionContext` used for asynchronous operations.
   * @return
   * A `Future[Boolean]` containing `true` if at least one element satisfies the predicate, or `false` otherwise.
   */
  @inline final def exists(p: V => Boolean)(using
      decrypt: Decrypt,
      ec: ExecutionContext
  ): Future[Boolean] =
    if isEmpty then Future.successful(false)
    else decrypted.map(p)

  /**
   * Checks if all elements of the decrypted collection satisfy the given predicate.
   * If the collection is empty, the result is `true`.
   *
   * @param p
   * A predicate function that evaluates elements of type `V` and returns a Boolean.
   * @param decrypt
   * An implicit `Decrypt` instance used to decrypt the collection.
   * @param ec
   * An implicit `ExecutionContext` used for asynchronous operations.
   * @return
   * A `Future` containing `true` if all elements satisfy the predicate or the collection is empty,
   * and `false` otherwise.
   */
  @inline final def forall(p: V => Boolean)(using
      decrypt: Decrypt,
      ec: ExecutionContext
  ): Future[Boolean] =
    if isEmpty then Future.successful(true)
    else decrypted.map(p)

  /** Applies a function to all elements of the decrypted collection.
    *
    * @param f
    *   The function to apply to each element.
    * @param decrypt
    *   The given decryption instance used to decrypt the collection.
    * @return
    *   Unit
    */
  @inline final def foreach[U](
      f: V => U
  )(using decrypt: Decrypt, ec: ExecutionContext): Unit =
    if !isEmpty then decrypted.foreach(f(_))

  /**
   * Folds the encrypted value into a single result by applying one of two provided functions:
   * one for the empty case and one for the non-empty case. If the value is empty, the `ifEmpty` function
   * is evaluated and returned. Otherwise, the value is decrypted and the provided function `f` is applied.
   *
   * @param ifEmpty
   * The computation to be performed when the encrypted value is empty.
   * @param f
   * The function to be applied to the decrypted value when it is non-empty.
   * @param decrypt
   * An implicit decryption instance used to decrypt the value.
   * @param ec
   * An implicit execution context for asynchronous processing.
   * @tparam W
   * The type of the result produced by folding the value.
   * @return
   * A `Future` containing the result of applying either `ifEmpty` or `f` on the decrypted value,
   * depending on whether the value is empty or not.
   */
  @inline final def fold[W: Codec](ifEmpty: => W)(f: V => W)(using
      decrypt: Decrypt,
      ec: ExecutionContext
  ): Future[W] =
    if isEmpty then Future.successful(ifEmpty) else decrypted.map(f)

  /**
   * Returns an asynchronous iterator over the decrypted collection.
   *
   * @param decrypt
   * The implicit `Decrypt` instance used to decrypt the collection.
   * @param ec
   * The implicit `ExecutionContext` used to run asynchronous operations.
   * @return
   * A `Future` containing an `Iterator` of type `V`. If the collection is empty,
   * it returns an empty iterator. Otherwise, it returns an iterator with the
   * decrypted value.
   */
  @inline final def iterator(using
      decrypt: Decrypt,
      ec: ExecutionContext
  ): Future[Iterator[V]] =
    if isEmpty then Future.successful(collection.Iterator.empty)
    else decrypted.map(collection.Iterator.single)

  /**
   * Decrypts the encrypted data (cipherText) and decodes it into a value of type `V`.
   *
   * @param decrypt an implicitly provided Decrypt instance used to decrypt the cipherText
   * @param ec      an implicitly provided ExecutionContext to run asynchronous operations
   * @return a Future containing the decoded value of type `V` if successful, or a failure if decryption or decoding fails
   */
  override def decrypted(using
      decrypt: Decrypt,
      ec: ExecutionContext
  ): Future[V] =
    decrypt(cipherText)
      .flatMap(ct =>
        summon[Codec[V]]
          .decode(ct)
          .fold[Future[V]](Future.failed, Future.successful)
      )

  /** Checks if the collection is empty.
    *
    * @return
    *   true if the collection contains no elements, false otherwise.
    */
  def isEmpty: Boolean = false

  /** Converts the current collection to a list.
    *
    * @param decrypt
    *   A given parameter for the decryption logic.
    * @return
    *   A `Try` containing a `List` of type `V`.
    */
  @inline final def toList(using
      decrypt: Decrypt,
      ec: ExecutionContext
  ): Future[List[V]] =
    if isEmpty then Future.successful(List()) else decrypted.map(_ :: Nil)
object Encrypted:

  /** Applies encryption to the given value using the provided codec and
    * encryptor.
    *
    * @param value
    *   The value to be encrypted.
    * @param encrypt
    *   a given encryption function that will be used to encrypt the encoded
    *   value.
    * @tparam V
    *   The type of the value being encrypted. A Codec must be available for
    *   this type.
    * @return
    *   An instance of Encrypted[V] containing the encrypted value. Returns an
    *   empty Encrypted instance if the input value is null.
    */
  def apply[V: Codec](
      value: V,
      manifest: Manifest
  )(using encrypt: Encrypt, ec: ExecutionContext): Future[Encrypted[V]] =
    if value == null then Future.successful(empty)
    else
      val plainText = summon[Codec[V]].encode(value, manifest)
      encrypt(plainText).map(t => Encrypted[V](t))

  /** Constructs an empty Encrypted instance.
    *
    * @return
    *   An instance of Encrypted representing an empty value.
    */
  def empty[V: Codec]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  def emptyF[V: Codec]: Future[Encrypted[V]] = Future.successful(empty)
  object Empty extends Encrypted[Nothing](CipherText.Empty):
    override def decrypted(using
        decrypt: Decrypt,
        executionContext: ExecutionContext
    ): Future[Nothing] = Future.failed(
      new UnsupportedOperationException("decrypted called on empty")
    )
    override def isEmpty: Boolean = true
