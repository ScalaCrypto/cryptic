package cryptic
package crypto

import java.security.*
import javax.crypto.Cipher
import scala.util.Try

/** Trait representing asymmetric encryption and decryption functionality.
  *
  * Provides a mechanism to encrypt and decrypt data using public and private
  * keys following asymmetric cryptographic principles.
  */
trait Asymmetric[F[_]]:
  
  def newCipher(mode: Int, key: Key): Cipher

  given encrypt(using key: PublicKey, functor: Functor[F]): Encrypt[F] =
    (plainText: PlainText) =>
      Try:
        val bytes = encrypt(plainText.bytes, key)
        CipherText(plainText.manifest, bytes)
      .lift

  def encrypt(bytes: IArray[Byte], key: PublicKey): IArray[Byte] =
    val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
    cipher.doFinal(bytes.mutable).immutable

  given decrypt(using key: PrivateKey, functor:Functor[F]): Decrypt[F] =
    (cipherText: CipherText) => {
      Try:
        val IArray(manifest, bytes) = cipherText.split
        val text = decrypt(bytes, key)
        PlainText(text, manifest)
      .lift
    }

  def decrypt(bytes: IArray[Byte], privateKey: PrivateKey): IArray[Byte] =
    val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, privateKey)
    cipher.doFinal(bytes.mutable).immutable
