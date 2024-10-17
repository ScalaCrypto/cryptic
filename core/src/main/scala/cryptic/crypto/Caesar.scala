package cryptic
package crypto

import scala.util.Try


/**
 * The `Caesar` object provides encryption and decryption functionality using the Caesar cipher.
 * It includes methods to generate keys, and implicit methods to encrypt and decrypt text using a specified key.
 *
 * The Caesar cipher is a type of substitution cipher in which each letter in the plaintext is shifted
 * a certain number of places down or up the alphabet based on the given offset.
 *
 * The `Key` case class ensures that the offset is non-zero.
 */
object Caesar {
  case class Key(offset: Int) {
    require(offset != 0)
  }
  implicit def encrypt(implicit key: Key): Encrypt = (plainText: PlainText) => {
    val bytes = plainText.map(b ⇒ (b + key.offset).toByte)
    CipherText(bytes)
  }
  implicit def decrypt(implicit key: Key): Decrypt = (cipherText: CipherText) =>
    Try[PlainText](PlainText(cipherText.bytes.map(b => (b - key.offset).toByte)))

  def keygen(offset: Int): Key = Key(offset)
}
