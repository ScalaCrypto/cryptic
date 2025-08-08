package cryptic

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

sealed abstract class Cryptic[V: Codec]:
  import Cryptic.*

  /** Decrypts the data using the provided implicit decryption mechanism.
    *
    * @param decrypt
    *   The implicit decryption logic that will be used to decrypt the data.
    * @return
    *   Returns a Try instance containing the decrypted value of type V if
    *   successful, or a Failure if the decryption fails.
    */
  def decrypted(using decrypt: Decrypt): Try[V]

  /** Attempts to retrieve the decrypted value. If decryption fails or if the
    * value is not present, it returns the provided default value.
    *
    * @param default
    *   the value to return if decryption fails or if the value is not present
    * @param decrypt
    *   an implicit decryption context
    * @return
    *   the decrypted value if successful, otherwise the default value
    */
  @inline final def decryptedOrElse[W >: V](default: => W)(using
      decrypt: Decrypt
  ): W = decrypted.getOrElse(default)

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
  import Encrypted.*
  sealed abstract class Operation[V: Codec] extends Cryptic[V]:
    def run(using encrypt: Encrypt, decrypt: Decrypt): Try[Encrypted[V]]
  sealed abstract class BinaryOperation[V: Codec, W: Codec]
      extends Operation[W]:
    override def run(using
        encrypt: Encrypt,
        decrypt: Decrypt
    ): Try[Encrypted[W]] =
      decrypted.map(w =>
        Encrypted(encrypt(implicitly[Codec[W]].encode(w)))
      )
  final case class Mapped[V: Codec, W: Codec](
      src: Cryptic[V],
      f: V => W
  ) extends BinaryOperation[V, W]:
    override def decrypted(using decrypt: Decrypt): Try[W] =
      src.decrypted.map(f)
  final case class FlatMapped[V: Codec, W: Codec](
      src: Cryptic[V],
      f: V => Cryptic[W]
  ) extends BinaryOperation[V, W]:
    override def decrypted(using decrypt: Decrypt): Try[W] =
      src.decrypted.flatMap[W](v => f(v).decrypted)
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
        decrypt: Decrypt
    ): Try[Encrypted[V]] = src.decrypted.map[Encrypted[V]]: v =>
      if pred(v) then v.encrypted else empty[V]
    override def decrypted(using decrypt: Decrypt): Try[V] =
      src.decrypted.flatMap: v =>
        if pred(v) then Success(v)
        else
          Failure(
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
        decrypt: Decrypt
    ): Try[Encrypted[W]] =
      src.decrypted match
        case Success(v) if pf.isDefinedAt(v) => Success(pf(v).encrypted)
        case Success(_)                      => Success(empty[W])
        case Failure(s)                      => Failure(s)
    override def decrypted(using decrypt: Decrypt): Try[W] =
      src.decrypted match
        case Success(v) if pf.isDefinedAt(v) => Success(pf(v))
        case Success(_) =>
          Failure(
            new UnsupportedOperationException(
              s"decrypted called on collected empty"
            )
          )
        case Failure(e) => Failure(e)
  final class OrElsed[V: Codec, W >: V: Codec](
      src: Cryptic[V],
      alternative: => Cryptic[W]
  ) extends BinaryOperation[V, W]:
    override def decrypted(using decrypt: Decrypt): Try[W] =
      src.decrypted match
        case r @ Success(_) => r
        case Failure(_)     => alternative.decrypted

/** Represents an encrypted value of type `V`.
  *
  * @param cipherText
  *   The encrypted data as `CipherText`.
  * @tparam V
  *   The type of the value being encrypted, requiring a given `Codec`
  *   instance.
  */
case class Encrypted[V: Codec](cipherText: CipherText) extends Cryptic[V]:

  /** Returns the byte array representation of the cipher text.
    *
    * @return
    *   the byte array of the cipher text
    */
  def bytes: Array[Byte] = cipherText.bytes

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

  /** Checks if the given value is contained within the object.
    *
    * @param value
    *   The value to be checked.
    * @param decrypt
    *   Implicit parameter to handle decryption.
    * @return
    *   True if the value is contained, false otherwise.
    */
  @inline final def contains(value: V)(using decrypt: Decrypt): Boolean =
    !isEmpty && decrypted == Try(value)

  /** Checks if there exists an element in the value that satisfies the
    * predicate `p`.
    *
    * @param p
    *   A predicate function that takes an element of type `V` and returns a
    *   Boolean.
    * @param decrypt
    *   A given parameter of type `Decrypt`, used to decrypt the value before
    *   applying the predicate.
    * @return
    *   A Boolean value that is true if the predicate holds for at least one
    *   element, false otherwise.
    */
  @inline final def exists(p: V => Boolean)(using
      decrypt: Decrypt
  ): Boolean =
    !isEmpty && decrypted.map(p).getOrElse(false)

  /** Tests whether a predicate holds for all elements after decryption.
    *
    * @param p
    *   the predicate to test elements against after decryption
    * @param decrypt
    *   a given decryption context
    * @return
    *   true if the container is empty or if the predicate holds for all
    *   decrypted elements, otherwise false
    */
  @inline final def forall(p: V => Boolean)(using
      decrypt: Decrypt
  ): Boolean =
    isEmpty || decrypted.map(p).getOrElse(true)

  /** Applies a function to all elements of the decrypted collection.
    *
    * @param f
    *   The function to apply to each element.
    * @param decrypt
    *   The given decryption instance used to decrypt the collection.
    * @return
    *   Unit
    */
  @inline final def foreach[U](f: V => U)(using decrypt: Decrypt): Unit =
    if !isEmpty then decrypted.foreach(f)

  /** Applies the provided functions based on the content of the collection.
    *
    * @param ifEmpty
    *   the function to apply if the collection is empty
    * @param f
    *   the function to apply if the collection contains a value
    * @param decrypt
    *   a given parameter providing decryption functionality
    * @return
    *   a `Try[W]` that represents the result of applying the appropriate
    *   function
    */
  @inline final def fold[W: Codec](ifEmpty: => W)(f: V => W)(using
                                                             decrypt: Decrypt
  ): Try[W] =
    if isEmpty then Try(ifEmpty) else decrypted.map(f)

  /** Provides an iterator over the elements of the collection. The iterator
    * will traverse the elements after decrypting them.
    *
    * @param decrypt
    *   a given parameter that handles the decryption of the elements
    * @return
    *   a `Try` that contains an `Iterator` of decrypted elements if successful,
    *   or a failure if decryption fails
    */
  @inline final def iterator(using decrypt: Decrypt): Try[Iterator[V]] =
    if isEmpty then Try(collection.Iterator.empty)
    else decrypted.map(collection.Iterator.single)

  /** Decrypts the cipher text using the provided `Decrypt` instance and
    * decodes it to type `V` using the given `Codec[V]`.
    *
    * @param decrypt
    *   a given parameter of type `Decrypt` to handle the decryption of the
    *   cipher text
    * @return
    *   a `Try[V]` containing the decoded value if successful, or a failure
    *   if either the decryption or decoding steps fail
    */
  override def decrypted(using decrypt: Decrypt): Try[V] =
    decrypt(cipherText).flatMap(implicitly[Codec[V]].decode)

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
  @inline final def toList(using decrypt: Decrypt): Try[List[V]] =
    if isEmpty then Try(List()) else decrypted.map(_ :: Nil)
object Encrypted:

  /** Applies encryption to the given value using the provided codec and
    * encryptor.
    *
    * @param value
    *   The value to be encrypted.
    * @param encrypt
    *   An given encryption function that will be used to encrypt the encoded
    *   value.
    * @tparam V
    *   The type of the value being encrypted. A Codec must be available
    *   for this type.
    * @return
    *   An instance of Encrypted[V] containing the encrypted value. Returns an
    *   empty Encrypted instance if the input value is null.
    */
  def apply[V: Codec](
      value: V
  )(using encrypt: Encrypt): Encrypted[V] =
    if value == null then empty
    else Encrypted[V](encrypt(implicitly[Codec[V]].encode(value)))

  /** Constructs an empty Encrypted instance.
    *
    * @return
    *   An instance of Encrypted representing an empty value.
    */
  def empty[V: Codec]: Encrypted[V] = Empty.asInstanceOf[Encrypted[V]]
  object Empty extends Encrypted[Nothing](CipherText.Empty):
    override def decrypted(implicit decrypt: Decrypt): Try[Nothing] = Failure(
      new UnsupportedOperationException("decrypted called on empty")
    )
    override def isEmpty: Boolean = true
