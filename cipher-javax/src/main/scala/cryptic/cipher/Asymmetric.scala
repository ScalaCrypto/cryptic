package cryptic
package cipher

import java.security.*
import javax.crypto.Cipher
import scala.util.Try

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

  given encrypt(using key: PublicKey, functor: Functor[F]): Encrypt[F] =
    (plainText: PlainText) =>
      // Todo move aad out of PlainText and make Encrypt trait check with types
      if plainText.aad.bytes.isEmpty then
        encrypt(plainText.bytes, key).map(CipherText(version.bytes, _))
      else
        functor.failed[CipherText](
          new UnsupportedOperationException(
            "Asymmetric ciphers do not support AAD"
          )
        )

  def encrypt(bytes: IArray[Byte], key: PublicKey)(using
      Functor[F]
  ): F[IArray[Byte]] =
    Try:
      val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
      cipher.doFinal(bytes.mutable).immutable
    .lift

  given decrypt(using key: PrivateKey, functor: Functor[F]): Decrypt[F] =
    (_: CipherText)
      .splitWith:
        case IArray(ver, encrypted) if version.supports(ver) =>
          decrypt(encrypted, key).map(PlainText.apply)
        case IArray(ver, _) =>
          version.failed(ver).lift

  def decrypt(bytes: IArray[Byte], privateKey: PrivateKey)(using
      functor: Functor[F]
  ): F[IArray[Byte]] =
    Try:
      val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, privateKey)
      cipher.doFinal(bytes.mutable).immutable
    .lift
