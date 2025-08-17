package cryptic
package crypto

import java.security.*
import javax.crypto.Cipher
import scala.util.Try

/** Asymetric trait provides encryption, decryption, and key generation
  * functionalities.
  *
  * @define encrypt
  *   Performs encryption on the given plain text using the provided public key.
  * @define decrypt
  *   Performs decryption on the given cipher text using the provided private
  *   key.
  * @define keygen
  *   Generates a key pair with the specified size.
  */
trait Asymmetric:
  // export java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

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
