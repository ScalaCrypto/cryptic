package cryptic

/** Represents a wrapper for signed data, encapsulating functionality for
 * verification and processing of the underlying signed text.
 *
 * @tparam F
 * The effect type, which must have a `Functor` instance.
 * @tparam V
 * The type of the value into which the verified data will be decoded, which
 * must have a `Codec` instance.
 * @param signedText
 * The effectful representation of the signed text.
 */
case class Signed[F[_]: Functor, V: Codec](signedText: F[SignedText]):
  
  def text:F[V] = signedText.flatMap(st => summon[Codec[V]].decode(PlainText(st.text)).lift)
  /**
   * Verifies the signed text using the contextual `Verify[F]` instance and decodes
   * the verified plain text into a value of type `V` using the `Codec` instance.
   *
   * Requires a contextual `Verify[F]` instance that provides the mechanism to verify
   * the signed text within the effect type `F` and return the verified plain text.
   *
   * @return
   *   The decoded value of type `V` wrapped in the effect type `F`, or a failure
   *   within the effect type `F` if verification or decoding fails.
   */
  def verified(using Verify[F]): F[V] =
    signedText
      .flatMap(_.verified)
      .flatMap(pt => summon[Codec[V]].decode(pt).lift)

  /** Applies a given partial function to process segments of the signed data,
    * producing an effectful computation of type `F[A]`.
    *
    * @param f
    *   A partial function that processes the segmented encrypted data,
    *   represented as `IArray[IArray[Byte]]`, and produces an effectful
    *   computation of type `F[A]`.
    *
    * @return
    *   The result of applying the provided partial function to the segmented
    *   data, wrapped in the effect type `F[A]`.
    */
  def splitWith[A](f: PartialFunction[IArray[IArray[Byte]], F[A]]): F[A] =
    signedText.flatMap(_.splitWith(f))

object Signed:
  def apply[F[_], V: Codec](signedText: SignedText)(using
      functor: Functor[F]
  ): Signed[F, V] =
    Signed(functor.pure(signedText))
  def deferred[F[_]: Functor, V: Codec](f: => F[SignedText]): Signed[F, V] =
    new Signed(f)
