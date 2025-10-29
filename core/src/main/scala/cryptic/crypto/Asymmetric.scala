package cryptic
package crypto

import java.security.*
import javax.crypto.Cipher
import scala.util.{Failure, Success, Try}

/** Represents an abstraction for asymmetric encryption and decryption operations
 * using a given effect type `F[_]`. Asymmetric encryption typically leverages
 * a pair of keys: a public key for encryption and a private key for decryption.
 *
 * This trait provides methods to perform encryption, decryption, and to create
 * initialized instances of `Cipher` for the required operation modes.
 *
 * @tparam F
 * The effect type wrapping the result of encryption and decryption. Examples
 * include `Try`, `Option`, or asynchronous data types like `IO`.
 */
trait Asymmetric[F[_]]:

  /** Creates a new Cipher instance configured for a specific mode and key.
    *
    * The method initializes a cryptographic Cipher object to perform encryption
    * or decryption operations based on the provided mode. The key specifies the
    * cryptographic key used in the operation.
    *
    * @param mode
    *   the mode for the Cipher (e.g., Cipher.ENCRYPT_MODE or
    *   Cipher.DECRYPT_MODE)
    * @param key
    *   the cryptographic key to be used for the operation
    * @return
    *   a configured Cipher instance ready to perform cryptographic operations
    */
  def newCipher(mode: Int, key: Key): Cipher

  /** Encodes the given encrypted byte array into a CipherText.
    *
    * @param encrypted
    *   The encrypted byte array to be encoded into a CipherText.
    * @return
    *   A CipherText representation of the provided encrypted byte array.
    */
  def encodeCipherText(encrypted: IArray[Byte]): CipherText

  /** Decodes the given cipher text represented as a nested array of bytes.
    *
    * This method is a partial function that takes an `IArray` of `IArray[Byte]`
    * as input. If the version within the input is supported, the function
    * extracts and returns the decrypted data wrapped in a functor `F`. If the
    * version is not supported, it produces an appropriate failure.
    *
    * The decoding typically involves processing metadata (like version) and
    * extracting the actual encrypted content.
    *
    * @return
    *   A partial function that maps a nested array of bytes to a encrypted
    *   result wrapped in a functor `F`, or a failure in case of an unsupported
    *   version.
    */
  def decodeCipherText: PartialFunction[IArray[IArray[Byte]], F[IArray[Byte]]]

  /** Retrieves the versioning information associated with the implementation.
    *
    * @return
    *   the versioning details as a `Versioning` instance.
    */
  val version: Version

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
      functor: Functor[F]
  ): F[IArray[Byte]] =
    Try:
      val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
      cipher.doFinal(bytes.mutable).immutable
    .lift

  given decrypt(using key: PrivateKey, functor: Functor[F]): Decrypt[F] =
    (_: CipherText)
      .splitWith(decodeCipherText)
      .flatMap(encrypted => {
        val decrypted = decrypt(encrypted, key)
        decrypted.map(PlainText.apply)
      })

  def decrypt(bytes: IArray[Byte], privateKey: PrivateKey)(using
      functor: Functor[F]
  ): F[IArray[Byte]] =
    Try:
      val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, privateKey)
      cipher.doFinal(bytes.mutable).immutable
    .lift
