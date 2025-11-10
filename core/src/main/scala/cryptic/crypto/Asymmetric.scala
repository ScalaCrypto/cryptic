package cryptic
package crypto

import java.security.*
import javax.crypto.Cipher
import scala.util.{Failure, Success, Try}

/** Represents an abstraction for asymmetric encryption and decryption
  * operations using a given effect type `F[_]`. Asymmetric encryption typically
  * leverages a pair of keys: a public key for encryption and a private key for
  * decryption.
  *
  * This trait provides methods to perform encryption, decryption, and to create
  * initialized instances of `Cipher` for the required operation modes.
  *
  * @tparam F
  *   The effect type wrapping the result of encryption and decryption. Examples
  *   include `Try`, `Option`, or asynchronous data types like `IO`.
  */
trait Asymmetric[F[_]]:

  /** Creates a new Cipher instance configured for a specific mode and key.
    *
    * The method initializes a cryptographic Cipher object to perform encryption
    * or decryption operations based on the provided mode. The key specifies the
    * cryptographic key used in the operation.
    *
    * @param mode
    *   the mode for the Cipher (i.e. Cipher.ENCRYPT_MODE or
    *   Cipher.DECRYPT_MODE)
    * @param key
    *   the cryptographic key to be used for the operation
    * @return
    *   a configured Cipher instance ready to perform cryptographic operations
    */
  def newCipher(mode: Int, key: Key): Cipher

  /** Retrieves the versioning information associated with the implementation.
    *
    * @return
    *   the versioning details as a `Versioning` instance.
    */
  val version: Version

  /**
   * Encodes the given encrypted payload into a `CipherText` object.
   *
   * This method combines the current version's bytes with the provided encrypted
   * data to produce a `CipherText` instance.
   *
   * @param encrypted
   * The encrypted payload as an immutable array of bytes.
   * @return
   * A `CipherText` instance containing the version bytes and the encrypted payload.
   */
  def encodeCipherText(encrypted: IArray[Byte]): CipherText =
    CipherText(version.bytes, encrypted)

  /** Decodes a cipher text payload using the provided functor. The method
   * supports specific versions of the payload format and applies transformations
   * based on the version compatibility.
   *
   * @param functor
   * the type class providing effectful computation capabilities for the
   * defined higher-kinded type `F`
   * @return
   * a partial function that matches an array of byte arrays, representing
   * the version and encrypted data. If the version is supported, it returns
   * the encrypted data wrapped in the effect `F`. If the version is not
   * supported, it fails the computation and lifts the failure into the effect
   * `F`.
   */
  def decodeCipherText(using
      functor: Functor[F]
  ): PartialFunction[IArray[IArray[Byte]], F[IArray[Byte]]] =
    case IArray(ver, encrypted) if version.supports(ver) =>
      functor.pure(encrypted)
    case IArray(ver, _) =>
      version.failed(ver).lift

  given encrypt(using key: PublicKey, functor: Functor[F]): Encrypt[F] =
    (plainText: PlainText) =>
      // Todo move aad out of PlainText and make Encrypt trait check with types
      if plainText.aad.nonEmpty then
        functor.failed[CipherText](
          new UnsupportedOperationException(
            "Asymmetric ciphers do not support AAD"
          )
        )
      else encrypt(plainText.bytes, key).map(encodeCipherText)

  def encrypt(bytes: IArray[Byte], key: PublicKey)(using
      Functor[F]
  ): F[IArray[Byte]] =
    Try:
      val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
      cipher.doFinal(bytes.mutable).immutable
    .lift

  given decrypt(using key: PrivateKey, functor: Functor[F]): Decrypt[F] =
    (_: CipherText)
      .splitWith(decodeCipherText)
      .flatMap(encrypted =>
        val decrypted = decrypt(encrypted, key)
        decrypted.map(PlainText.apply)
      )

  def decrypt(bytes: IArray[Byte], privateKey: PrivateKey)(using
      functor: Functor[F]
  ): F[IArray[Byte]] =
    Try:
      val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, privateKey)
      cipher.doFinal(bytes.mutable).immutable
    .lift
