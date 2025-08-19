package cryptic
package crypto

import java.security.*
import javax.crypto.Cipher
import scala.util.Try

/**
 * Trait representing asymmetric encryption and decryption functionality.
 *
 * Provides a mechanism to encrypt and decrypt data using public and private keys
 * following asymmetric cryptographic principles.
 */
trait Asymmetric:
  def newCipher(mode: Int, key: Key): Cipher

  given encrypt(using key: PublicKey): Encrypt =
    (plainText: PlainText) =>
      val encrypted = encrypt(plainText.bytes, key)
      CipherText(plainText.manifest, encrypted)

  def encrypt(bytes: IArray[Byte], key: PublicKey): IArray[Byte] =
    val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
    cipher.doFinal(bytes.mutable).immutable

  given decrypt(using key: PrivateKey): Decrypt =
    (cipherText: CipherText) =>
      Try:
        val IArray(manifest, bytes) = cipherText.split
        val text = decrypt(bytes, key)
        PlainText(text, manifest)

  def decrypt(bytes: IArray[Byte], privateKey: PrivateKey): IArray[Byte] =
    val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, privateKey)
    cipher.doFinal(bytes.mutable).immutable
