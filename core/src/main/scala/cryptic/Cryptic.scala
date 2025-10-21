package cryptic

/** Core abstraction representing a value of type `V` which may be encrypted and
  * can be operated on within an effect `F`.
  *
  * Cryptic provides a small set of combinators
  * (map/flatMap/filter/collect/orElse) that allow building computation
  * pipelines without forcing decryption up‑front. Actual decryption happens
  * only when `decrypted` (or operations derived from it) is evaluated with an
  * in‑scope `Decrypt[F]` instance.
  *
  * Type parameters and context bounds:
  *   - F: an effect type with a given `Functor[F]`
  *   - V: the value type with a given `Codec[V]` used for encoding/decoding
  *     when encrypting/decrypting
  */
sealed abstract class Cryptic[F[_]: Functor, V: Codec]:
  import Cryptic.*

  /** Attempts to retrieve the decrypted value using the provided decryption
    * context.
    *
    * @param decrypt
    *   the decryption context used to perform the decryption
    * @return
    *   the decrypted value wrapped in the effect type `F`
    */
  def decrypted(using decrypt: Decrypt[F]): F[V]

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
      decrypt: Decrypt[F]
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
  @inline final def map[W: Codec](f: V => W): Operation[F, W] =
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
  @inline final def flatMap[W: Codec](
      f: V => Cryptic[F, W]
  ): Operation[F, W] =
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
  @inline final def filter(p: V => Boolean): Operation[F, V] = Filtered(this, p)

  /** Filters out elements of a collection that satisfy a given predicate.
    *
    * @param p
    *   the predicate function used to test elements, excluding those that
    *   return true
    * @return
    *   a new collection without the elements that satisfy the predicate
    */
  @inline final def filterNot(p: V => Boolean): Filtered[F, V] =
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
  ): Operation[F, W] = Collected(this, pf)

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
      alternative: => Cryptic[F, W]
  ): Operation[F, W] = new OrElsed(this, alternative)

/** Companion utilities and data types for building `Cryptic` programs.
  *
  * Exposes constructors, operation types, and convenient exports for the
  * encryption/decryption type classes and data types.
  */
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

  /** A deferred cryptic computation producing a value of type `V`.
    *
    * Operations are composable descriptions that can be interpreted via `run`,
    * which performs encryption using the given instances, or via `decrypted`,
    * which performs decryption to obtain the plain value.
    */
  sealed abstract class Operation[F[_]: Functor, V: Codec]
      extends Cryptic[F, V]:
    def run(using
        encrypt: Encrypt[F],
        decrypt: Decrypt[F]
    ): Encrypted[F, V]

  /** An `Operation` that derives from a single source `Cryptic[F, V]` and
    * produces a result of type `W`.
    *
    * Implementations typically transform or branch based on the decrypted
    * source.
    */
  sealed abstract class BinaryOperation[F[_]: Functor, V: Codec, W: Codec]
      extends Operation[F, W]:
    override def run(using
        encrypt: Encrypt[F],
        decrypt: Decrypt[F]
    ): Encrypted[F, W] =
      val t = decrypted
      Encrypted(t, AAD.empty.pure)

  /** Result of mapping a decrypted value of type `V` to `W`.
    *
    * The source is not decrypted until interpreted via `decrypted`/`run`.
    * Mapping uses the provided pure function `f`.
    */
  final case class Mapped[F[_]: Functor, V: Codec, W: Codec](
      src: Cryptic[F, V],
      f: V => W
  ) extends BinaryOperation[F, V, W]:
    override def decrypted(using decrypt: Decrypt[F]): F[W] =
      src.decrypted.map(f)

  /** Result of flatMapping a decrypted value of type `V` into a new
    * `Cryptic[F, W]`.
    *
    * This composes two cryptic computations without decrypting eagerly.
    */
  final case class FlatMapped[F[_]: Functor, V: Codec, W: Codec](
      src: Cryptic[F, V],
      f: V => Cryptic[F, W]
  ) extends BinaryOperation[F, V, W]:
    override def decrypted(using decrypt: Decrypt[F]): F[W] =
      src.decrypted.map(v => f(v)).flatMap(w => w.decrypted)
  /*
  final case class Flattened[V : Codec, W : Codec](src: Cryptic[V])(using ev: V <:< Cryptic[W])
      extends Operation[V]:
    override def run(using encrypt: Encrypt, decrypt: Decrypt): Try[Encrypted[V]] =
      src.decrypted.map[Encrypted[V]](ev.apply)
    override def decrypted(using decrypt: Decrypt): Try[W] =
      src.decrypted.flatMap(v => ev(v).decrypted)
   */

  /** Filters a decrypted value using predicate `pred`.
    *
    * If the predicate does not hold, the encrypted representation becomes empty
    * when interpreted with `run`, and `decrypted` will fail when forced.
    */
  final case class Filtered[F[_]: Functor, V: Codec](
      src: Cryptic[F, V],
      pred: V => Boolean
  ) extends Operation[F, V]:
    override def run(using
        encrypt: Encrypt[F],
        decrypt: Decrypt[F]
    ): Encrypted[F, V] =
      val filtered: F[CipherText] = src.decrypted.flatMap: v =>
        if pred(v) then encrypt(PlainText.encode(v, AAD.empty))
        else CipherText.empty.pure
      new Encrypted(filtered)

    override def decrypted(using
        decrypt: Decrypt[F]
    ): F[V] =
      src.decrypted.flatMap: v =>
        if pred(v) then v.pure
        else
          new UnsupportedOperationException(
            "decrypted called on filtered empty"
          ).failed

  /** Applies a partial function `pf` to a decrypted value, producing a `W` when
    * defined.
    *
    * If `pf` is not defined for the decrypted value, the operation behaves as
    * empty when interpreted with `run` and `decrypted` will fail when forced.
    */
  final case class Collected[F[_]: Functor, V: Codec, W: Codec](
      src: Cryptic[F, V],
      pf: PartialFunction[V, W]
  ) extends BinaryOperation[F, V, W]:
    override def run(using
        encrypt: Encrypt[F],
        decrypt: Decrypt[F]
    ): Encrypted[F, W] =
      val collected = src.decrypted.flatMap: v =>
        if pf.isDefinedAt(v) then encrypt(PlainText.encode(v, AAD.empty))
        else CipherText.empty.pure
      new Encrypted(collected)

    override def decrypted(using
        decrypt: Decrypt[F]
    ): F[W] =
      src.decrypted.flatMap: v =>
        if pf.isDefinedAt(v) then pf(v).pure
        else
          new UnsupportedOperationException(
            s"decrypted called on collected empty"
          ).failed

  /** Represents a fall-back computation: if `src` fails to decrypt, evaluate
    * `alternative`.
    */
  final class OrElsed[F[_]: Functor, V: Codec, W >: V: Codec](
      src: Cryptic[F, V],
      alternative: => Cryptic[F, W]
  ) extends BinaryOperation[F, V, W]:
    override def decrypted(using
        decrypt: Decrypt[F]
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
case class Encrypted[F[_]: Functor, V: Codec](cipherText: F[CipherText])
    extends Cryptic[F, V]:

  /** Returns the byte array representation of the cipher text.
    *
    * @return
    *   the byte array of the cipher text
    */
  def bytes: F[IArray[Byte]] = cipherText.map(_.bytes)

  /** Checks if this encrypted value is non-empty (defined).
    *
    * @return
    *   true if a value is present, false otherwise.
    */
  @inline final def nonEmpty: F[Boolean] = isDefined

  /** Checks if the value is defined.
    *
    * @return
    *   true if the value is defined, false otherwise
    */
  @inline final def isDefined: F[Boolean] = isEmpty.map(!_)

  /** Checks if the given value is contained within the object.
    *
    * @param value
    *   The value to be checked.
    * @param decrypt
    *   given parameter to handle decryption.
    * @return
    *   True if the value is contained, false otherwise.
    */
  @inline final def contains(value: V)(using decrypt: Decrypt[F]): F[Boolean] =
    ifEmpty(false.pure)(decrypted.map(_ == value))

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
      decrypt: Decrypt[F]
  ): F[Boolean] = ifEmpty(false.pure)(decrypted.map(p))

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
      decrypt: Decrypt[F]
  ): F[Boolean] = ifEmpty(true.pure)(decrypted.map(p))

  /** Applies a function to all elements of the decrypted collection.
    *
    * @param f
    *   The function to apply to each element.
    * @param decrypt
    *   The given decryption instance used to decrypt the collection.
    * @return
    *   Unit
    */
  @inline final def foreach(f: V => Unit)(using decrypt: Decrypt[F]): Unit =
    isEmpty.foreach(_ => decrypted.foreach(f))
//    ifEmpty(_=>Unit)(decrypted.foreach(f))

  /** Folds the encrypted value into a result of type `W` by applying one of two
    * provided functions depending on whether the encrypted value is empty or
    * non-empty.
    *
    * If the value is empty, the `empty` computation is returned wrapped in the
    * effect type `F`. If the value is non-empty, it is first decrypted and then
    * transformed using the function `f`.
    *
    * @param empty
    *   A computation yielding a default value of type `W` to be used when the
    *   encrypted value is empty.
    * @param f
    *   A function that transforms the decrypted value of type `V` into a result
    *   of type `W`.
    * @param decrypt
    *   The decryption context used to decrypt the encrypted value.
    * @return
    *   An effectful computation of type `F[W]` representing the folded result.
    */
  @inline final def fold[W: Codec](empty: => W)(f: V => W)(using
      decrypt: Decrypt[F]
  ): F[W] = ifEmpty(empty.pure)(decrypted.map(f))

  /** Retrieves an iterator over the decrypted collection.
    *
    * If the collection is empty, the iterator will also be empty. Otherwise,
    * the collection is decrypted, and the result is provided as a
    * single-element iterator.
    *
    * @param decrypt
    *   A given instance of the `Decrypt` type class, responsible for decrypting
    *   the encrypted data.
    * @return
    *   An effectful computation of type `F[Iterator[V]]` containing an iterator
    *   over the decrypted values.
    */
  @inline final def iterator(using decrypt: Decrypt[F]): F[Iterator[V]] =
    ifEmpty(collection.Iterator.empty.pure):
      decrypted.map(collection.Iterator.single)

  /** Decrypts the encrypted data to retrieve the original value of type `V`.
    *
    * This method checks if the container is empty. If empty, it raises an
    * `UnsupportedOperationException`. Otherwise, it decrypts the ciphertext,
    * decodes the decrypted content using a `Codec` instance, and lifts the
    * result into the effect type `F`.
    *
    * @param decrypt
    *   An implicit parameter for the `Decrypt` type class, which specifies the
    *   decryption logic for the effect type `F`.
    * @return
    *   The decrypted value of type `V`, wrapped in the effect type `F`. If the
    *   container is empty, it raises an exception wrapped in `F`.
    */
  override def decrypted(using decrypt: Decrypt[F]): F[V] =
    ifEmpty(
      new UnsupportedOperationException("decrypted called on empty").failed
    )(
      cipherText
        .flatMap(ct => decrypt(ct))
        .flatMap(pt => summon[Codec[V]].decode(pt).lift)
    )

  /** Checks if the collection is empty.
    *
    * @return
    *   true if the collection contains no elements, false otherwise.
    */
  def isEmpty: F[Boolean] = cipherText.map(_.isEmpty)

  /** Converts the encrypted value into a list containing the decrypted value.
    *
    * If the container is empty, it returns a list with no elements. If the
    * container is non-empty, it decrypts the contained value and wraps it in a
    * single-element list.
    *
    * @param decrypt
    *   An implicit parameter providing the decryption logic for the effect type
    *   `F`.
    * @return
    *   An effectful computation of type `F[List[V]]` containing the decrypted
    *   value wrapped in a list, or an empty list if the container is empty.
    */
  @inline final def toList(using decrypt: Decrypt[F]): F[List[V]] =
    ifEmpty(Nil.pure):
      decrypted.map(_ :: Nil)

  def ifEmpty[A](whenEmpty: => F[A])(whenNonEmpty: => F[A]): F[A] =
    isEmpty.flatMap(empty => if empty then whenEmpty else whenNonEmpty)

object Encrypted:

  /** Applies encryption to the provided value using a given aad and returns an
    * encrypted representation.
    *
    * @param value
    *   The value to be encrypted.
    * @param aad
    *   The aad describing additional properties required to encode the value.
    * @param encrypt
    *   The encryption implementation that handles encoding the plain text into
    *   cipher text.
    * @tparam F
    *   The effect type representing how the operations are to be executed.
    * @tparam V
    *   The type of the value to be encrypted.
    * @return
    *   An instance of `Encrypted[F, V]` containing the encrypted representation
    *   of the input value within the given effect type.
    */
  def apply[F[_]: Functor, V: Codec](value: V, aad: AAD)(using
      encrypt: Encrypt[F]
  ): Encrypted[F, V] =
    val ct =
      if value == null then CipherText.empty.pure
      else encrypt(PlainText.encode(value, aad))
    new Encrypted(ct)

  /** Encrypts an effectful value `value: F[V]` using an effectful `AAD`. Each
    * pair `(v, mf)` is encoded and encrypted within the effect `F`.
    */
  def apply[F[_]: Functor, V: Codec](value: F[V], aad: F[AAD])(using
      encrypt: Encrypt[F]
  ): Encrypted[F, V] =
    val cipherText = for
      v <- value
      mf <- aad
      encrypted <- encrypt(PlainText.encode(v, mf))
    yield encrypted
    new Encrypted(cipherText)

  /** Constructs an `Encrypted` value from a precomputed cipher text effect.
    *
    * Use this when you already have `F[CipherText]` and want to wrap it as
    * `Encrypted[F, V]`.
    */
  def deferred[F[_]: Functor, V: Codec](f: => F[CipherText]): Encrypted[F, V] =
    new Encrypted(f)

  /** Creates an empty `Encrypted` value for type `V` within effect `F`.
    *
    * The resulting value behaves as absent/empty: `isEmpty` is true, `run`
    * yields `CipherText.empty`, and `decrypted` will fail when forced.
    */
  def empty[F[_]: Functor, V: Codec]: Encrypted[F, V] =
    new Encrypted(CipherText.empty.pure)
