package cryptic

sealed abstract class Cryptic[V: Codec]:
  import Cryptic.*

  /** Attempts to retrieve the decrypted value using the provided decryption
    * context.
    *
    * @param decrypt
    *   the decryption context used to perform the decryption
    * @return
    *   the decrypted value wrapped in the effect type `F`
    */
  def decrypted[F[_]](using decrypt: Decrypt[F], functor: Functor[F]): F[V]

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
  @inline final def decryptedOrElse[F[_], W >: V](default: => W)(using
      decrypt: Decrypt[F],
      functor: Functor[F]
  ): F[W] = decrypted.recoverWith:
    case _: Throwable => default.pure

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
  ): Operation[W] = new OrElsed(this, alternative)

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
    def run[E[_], D[_]](using
        encrypt: Encrypt[E],
        decrypt: Decrypt[D],
        functorE: Functor[E],
        functorD: Functor[D]
    ): E[Encrypted[V]]

  sealed abstract class BinaryOperation[V: Codec, W: Codec]
      extends Operation[W]:
    override def run[E[_], D[_]](using
        encrypt: Encrypt[E],
        decrypt: Decrypt[D],
        functorE: Functor[E],
        functorD: Functor[D]
    ): E[Encrypted[W]] = decrypted.transform[E].flatMap(_.encrypted)

  final case class Mapped[V: Codec, W: Codec](
      src: Cryptic[V],
      f: V => W
  ) extends BinaryOperation[V, W]:
    override def decrypted[F[_]](using
        decrypt: Decrypt[F],
        functor: Functor[F]
    ): F[W] = src.decrypted.map(f)

  final case class FlatMapped[V: Codec, W: Codec](
      src: Cryptic[V],
      f: V => Cryptic[W]
  ) extends BinaryOperation[V, W]:
    override def decrypted[F[_]](using
        decrypt: Decrypt[F],
        functor: Functor[F]
    ): F[W] = src.decrypted.flatMap(v => f(v).decrypted)
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
    override def run[E[_], D[_]](using
        encrypt: Encrypt[E],
        decrypt: Decrypt[D],
        functorE: Functor[E],
        functorD: Functor[D]
    ): E[Encrypted[V]] = src.decrypted
      .transform[E]
      .flatMap: v =>
        if pred(v) then v.encrypted else empty[V, E]

    override def decrypted[F[_]](using
        decrypt: Decrypt[F],
        functor: Functor[F]
    ): F[V] =
      src.decrypted.flatMap: v =>
        if pred(v) then v.pure
        else
          new UnsupportedOperationException(
            "decrypted called on filtered empty"
          ).failed

  final case class Collected[V: Codec, W: Codec](
      src: Cryptic[V],
      pf: PartialFunction[V, W]
  ) extends BinaryOperation[V, W]:
    override def run[E[_], D[_]](using
        encrypt: Encrypt[E],
        decrypt: Decrypt[D],
        functorE: Functor[E],
        functorD: Functor[D]
    ): E[Encrypted[W]] =
      src.decrypted
        .transform[E]
        .flatMap:
          case v: V if pf.isDefinedAt(v) => pf(v).encrypted
          case _                         => empty[W, E]

    override def decrypted[F[_]](using
        decrypt: Decrypt[F],
        functor: Functor[F]
    ): F[W] =
      src.decrypted.flatMap:
        case v: V if pf.isDefinedAt(v) => pf(v).pure
        case _ =>
          new UnsupportedOperationException(
            s"decrypted called on collected empty"
          ).failed

  final class OrElsed[V: Codec, W >: V: Codec](
      src: Cryptic[V],
      alternative: => Cryptic[W]
  ) extends BinaryOperation[V, W]:
    override def decrypted[F[_]](using
        decrypt: Decrypt[F],
        functor: Functor[F]
    ): F[W] =
      src.decrypted.recoverWith:
        case _ => alternative.decrypted

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

  /** Checks if the given value is contained within the object.
    *
    * @param value
    *   The value to be checked.
    * @param decrypt
    *   given parameter to handle decryption.
    * @return
    *   True if the value is contained, false otherwise.
    */
  @inline final def contains[F[_]](
      value: V
  )(using decrypt: Decrypt[F], functor: Functor[F]): F[Boolean] =
    if isEmpty then false.pure else decrypted.map(_ == value)

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
  @inline final def exists[F[_]](p: V => Boolean)(using
      decrypt: Decrypt[F],
      functor: Functor[F]
  ): F[Boolean] =
    if isEmpty then false.pure else decrypted.map(p)

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
  @inline final def forall[F[_]](p: V => Boolean)(using
      decrypt: Decrypt[F],
      functor: Functor[F]
  ): F[Boolean] =
    if isEmpty then true.pure else decrypted.map(p)

  /** Applies a function to all elements of the decrypted collection.
    *
    * @param f
    *   The function to apply to each element.
    * @param decrypt
    *   The given decryption instance used to decrypt the collection.
    * @return
    *   Unit
    */
  @inline final def foreach[F[_], U](f: V => U)(using
      decrypt: Decrypt[F],
      functor: Functor[F]
  ): Unit =
    if !isEmpty then decrypted.map(f)

  /** Folds the encrypted value into a result of type `W`. If the value is
    * empty, returns a default value provided by `ifEmpty`. Otherwise, decrypts
    * the value and applies the function `f` to produce the result.
    *
    * @param ifEmpty
    *   The default value to use if the encrypted value is empty.
    * @param f
    *   A function to transform the decrypted value of type `V` into a result of
    *   type `W`.
    * @param decrypt
    *   The decryption context used to decrypt the encrypted value.
    * @param functor
    *   The functor type class providing the necessary effect operations.
    * @return
    *   An effectful computation containing the folded result of type `W`.
    */
  @inline final def fold[F[_], W: Codec](ifEmpty: => W)(f: V => W)(using
      decrypt: Decrypt[F],
      functor: Functor[F]
  ): F[W] =
    if isEmpty then ifEmpty.pure else decrypted.map(f)

  /** Provides an iterator over the decrypted values of type `V`. If the
    * collection is empty, returns an empty iterator. Otherwise, decrypts the
    * value and wraps it into a singleton iterator.
    *
    * @param decrypt
    *   The decryption context used for decrypting the value.
    * @param functor
    *   The functor instance providing the necessary effect operations.
    * @return
    *   An effectful computation containing an iterator over the decrypted
    *   value(s).
    */
  @inline final def iterator[F[_]](using
      decrypt: Decrypt[F],
      functor: Functor[F]
  ): F[Iterator[V]] =
    if isEmpty then collection.Iterator.empty.pure
    else decrypted.map(collection.Iterator.single)

  /** Decrypts the cipher text and decodes it into the value of type `V`.
    *
    * @param decrypt
    *   A given instance of the `Decrypt` type class, used to perform decryption
    *   operations.
    * @param functor
    *   A given functor instance providing the necessary effect operations for
    *   the effect type `F[_]`.
    * @return
    *   An effectful computation containing the decrypted and decoded value of
    *   type `V`.
    */
  override def decrypted[F[_]](using
      decrypt: Decrypt[F],
      functor: Functor[F]
  ): F[V] =
    decrypt(cipherText).flatMap(pt => summon[Codec[V]].decode(pt).lift)

  /** Checks if the collection is empty.
    *
    * @return
    *   true if the collection contains no elements, false otherwise.
    */
  def isEmpty: Boolean = false

  /** Converts the encrypted content into a list of decrypted values.
    *
    * If the collection is empty, this method returns an effectful computation
    * wrapping an empty list. Otherwise, it decrypts the value and wraps it into
    * a singleton list.
    *
    * @param decrypt
    *   An instance of the `Decrypt` type class, used to perform decryption
    *   operations.
    * @param functor
    *   The functor instance enabling effectful transformations for the effect
    *   type `F[_]`.
    * @return
    *   An effectful computation wrapping a list of values of type `V`.
    */
  @inline final def toList[F[_]](using
      decrypt: Decrypt[F],
      functor: Functor[F]
  ): F[List[V]] =
    if isEmpty then Nil.pure else decrypted.map(_ :: Nil)

object Encrypted:

  /** Encrypts the given value into an instance of `Encrypted[V]` within the
    * effect type `F`.
    *
    * @param value
    *   The value of type `V` to be encrypted. A `Codec` instance for type `V`
    *   must be provided.
    * @param manifest
    *   The `Manifest` representing type metadata for encoding the value.
    * @param encrypt
    *   A given type class instance providing encryption logic within the effect
    *   type `F`.
    * @param functor
    *   A given `Functor` instance for handling effectful computations.
    * @return
    *   An effect encapsulating the encrypted value as an instance of
    *   `Encrypted[V]`, or an empty instance if the value is `null`.
    */
  def apply[V: Codec, F[_]](value: V, manifest: Manifest)(using
      encrypt: Encrypt[F],
      functor: Functor[F]
  ): F[Encrypted[V]] =
    if value == null then empty[V, F]
    else
      val plainText = summon[Codec[V]].encode(value, manifest)
      encrypt(plainText).map(ct => Encrypted[V](ct))

  /** Constructs an empty Encrypted instance.
    *
    * @return
    *   An instance of Encrypted representing an empty value.
    */
  def empty[V: Codec, F[_]](using functor: Functor[F]): F[Encrypted[V]] =
    Empty.asInstanceOf[Encrypted[V]].pure

  object Empty extends Encrypted[Nothing](CipherText.Empty):
    override def decrypted[F[_]](using
        decrypt: Decrypt[F],
        functor: Functor[F]
    ): F[Nothing] =
      new UnsupportedOperationException("decrypted called on empty").failed

    override def isEmpty: Boolean = true
