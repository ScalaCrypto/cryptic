package cryptic
package crypto

import java.security.*
import javax.crypto.Cipher
import scala.util.{Failure, Try}

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

  def newCipher(mode: Int, key: Key): Cipher

  given encrypt(using key: PublicKey, functor: Functor[F]): Encrypt[F] =
    (plainText: PlainText) =>
      // Todo move aad out of PlainText and make Encrypt trait check with types
      if plainText.aad.nonEmpty then
        functor.failed[CipherText](
          new UnsupportedOperationException(
            "Asymmetric ciphers do not support AAD"
          )
        )
      else encrypt(plainText.bytes, key).map(CipherText.apply)

  def encrypt(bytes: IArray[Byte], key: PublicKey)(using
      Functor[F]
  ): F[IArray[Byte]] =
    Try:
      newCipher(Cipher.ENCRYPT_MODE, key)
        .doFinal(bytes.mutable)
        .immutable
    .lift

  given decrypt(using key: PrivateKey, functor: Functor[F]): Decrypt[F] =
    (cipherText: CipherText) =>
      decrypt(cipherText.bytes, key).map(PlainText.apply)

  def decrypt(bytes: IArray[Byte], privateKey: PrivateKey)(using Functor[F]): F[IArray[Byte]] =
    Try:
      newCipher(Cipher.DECRYPT_MODE, privateKey).doFinal(bytes.mutable).immutable
    .lift
