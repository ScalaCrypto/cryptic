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
      else
        Try:
          val bytes = encrypt(plainText.bytes, key)
          CipherText(bytes)
        .lift

  def encrypt(bytes: IArray[Byte], key: PublicKey): IArray[Byte] =
    val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
    cipher.doFinal(bytes.mutable).immutable

  given decrypt(using key: PrivateKey, functor: Functor[F]): Decrypt[F] =
    (cipherText: CipherText) =>
      Try:
        val text = decrypt(cipherText.bytes, key)
        PlainText(text)
      .lift

  def decrypt(bytes: IArray[Byte], privateKey: PrivateKey): IArray[Byte] =
    val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, privateKey)
    cipher.doFinal(bytes.mutable).immutable
