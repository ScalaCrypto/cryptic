package cryptic
package crypto

import java.security.*
import javax.crypto.Cipher
import scala.util.{Failure, Try}

/** Trait representing asymmetric encryption and decryption functionality.
  *
  * Provides a mechanism to encrypt and decrypt data using public and private
  * keys following asymmetric cryptographic principles.
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

  /** Retrieves the versioning information associated with the implementation.
    *
    * @return
    *   the versioning details as a `Versioning` instance.
    */
  def version: Versioning
  given Versioning = version

  given encrypt(using key: PublicKey, functor: Functor[F]): Encrypt[F] =
    (plainText: PlainText) =>
      // Todo move aad out of PlainText and make Encrypt trait check with types
      if plainText.aad.nonEmpty then
        functor.failed[CipherText](
          new UnsupportedOperationException(
            "Asymmetric ciphers do not support AAD"
          )
        )
      else
        Try:
          val bytes = encrypt(plainText.bytes, key)
          CipherText(version.bytes, bytes)
        .lift

  def encrypt(bytes: IArray[Byte], key: PublicKey): IArray[Byte] =
    val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
    cipher.doFinal(bytes.mutable).immutable

  given decrypt(using key: PrivateKey, functor: Functor[F]): Decrypt[F] =
    (_: CipherText)
      .withVersion:
        case IArray(_, encrypted) => PlainText(decrypt(encrypted, key))
      .lift

  def decrypt(bytes: IArray[Byte], privateKey: PrivateKey): IArray[Byte] =
    val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, privateKey)
    cipher.doFinal(bytes.mutable).immutable
