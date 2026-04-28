package cryptic
package cipher

import cryptic.{Functor, PlainText, Sign, SignedText, Verify, Version}
import java.security.{PrivateKey, PublicKey, Signature}

/** The `Signer` trait provides utilities to create and validate digital
  * signatures.
  *
  * @tparam F
  *   a higher-kinded type that represents the context or the effect in which
  *   the computation operates
  */
trait Signer[F[_]]:

  /** Retrieves the versioning information associated with the implementation.
    *
    * @return
    *   the versioning details as a `Versioning` instance.
    */
  val version: Version

  /** Generates a new digital signature encapsulated within the specified
    * effectful computation.
    *
    * The method abstracts the creation of a `Signature` instance by leveraging
    * a functor-based effect type. This is useful in functional programming
    * contexts where computations are composed and executed within an effectful
    * container.
    *
    * @return
    *   An effectful computation containing the generated `Signature`.
    */
  def newSignature: F[Signature]

  /** Signs the given plain text using the provided private key and creates a
    * signed text.
    *
    * @param key
    *   the private key used for signing the plain text
    * @param functor
    *   the functor used to encapsulate the effectful computation
    * @return
    *   an instance of `Sign[F]`, which is a function that takes a `PlainText`
    *   and returns an effectful computation of the resulting `SignedText`
    */
  given sign(using
      key: PrivateKey,
      functor: Functor[F]
  ): Sign[F] =
    (plainText: PlainText) =>
      val signature = sign(plainText.bytes, key)
      signature.map(SignedText(version.bytes, plainText.bytes, _))

  /** Signs the given immutable array of bytes using the provided private key.
    *
    * This method creates a digital signature by applying the provided private
    * key to the given bytes, encapsulating the result in the specified
    * effectful computation."
    *
    * @param bytes
    *   the data to be signed, represented as an immutable array of bytes
    * @param key
    *   the private key used to sign the given data
    * @param functor
    *   the functor providing support for effectful computations
    * @return
    *   an effectful computation containing the signed data, represented as an
    *   immutable array of bytes
    */
  def sign(bytes: IArray[Byte], key: PrivateKey)(using
      functor: Functor[F]
  ): F[IArray[Byte]] = newSignature.map: signature =>
    signature.initSign(key)
    signature.update(version.bytes.mutable)
    signature.update(bytes.mutable)
    signature.sign().immutable

  /** Verifies a signed text using the provided public key and returns the plain
    * text if verification is successful.
    *
    * @param key
    *   The public key used to verify the signature.
    * @param functor
    *   The functor that encapsulates the effectful computation.
    * @return
    *   An instance of `Verify[F]`, which is a function that takes a
    *   `SignedText` and returns an effectful computation containing the
    *   verified `PlainText` or a failure if verification fails.
    */
  given verify(using key: PublicKey, functor: Functor[F]): Verify[F] =
    (_: SignedText)
      .splitWith:
        case IArray(ver, bytes, sig) if version.supports(ver) =>
          verify(ver, bytes, sig, key).map(PlainText.apply)
        case IArray(ver, _, _) =>
          version.failed(ver).lift

  /** Verifies a digital signature using the provided public key and returns the
    * original data if the signature is valid.
    *
    * The method ensures the integrity and authenticity of the signed data by
    * checking the provided signature against the data and the public key. If
    * verification fails, an appropriate error is encapsulated within the
    * effect.
    *
    * @param bytes
    *   The data whose signature is to be verified, represented as an immutable
    *   array of bytes.
    * @param sigBytes
    *   The digital signature to be verified, represented as an immutable array
    *   of bytes.
    * @param publicKey
    *   The public key used to verify the digital signature.
    * @param functor
    *   The functor providing support for handling effectful computations.
    * @return
    *   An effectful computation containing the original data as an immutable
    *   array of bytes if the signature is valid, or a failure effect if the
    *   verification fails.
    */
  def verify(
      version: IArray[Byte],
      bytes: IArray[Byte],
      sigBytes: IArray[Byte],
      publicKey: PublicKey
  )(using
      functor: Functor[F]
  ): F[IArray[Byte]] = newSignature.flatMap(signature =>
    signature.initVerify(publicKey)
    signature.update(version.mutable)
    signature.update(bytes.mutable)
    if signature.verify(sigBytes.mutable) then functor.pure(bytes)
    else
      functor.failed(
        new IllegalArgumentException("Signature verification failed")
      )
  )
