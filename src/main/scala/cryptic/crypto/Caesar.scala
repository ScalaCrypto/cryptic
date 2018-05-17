package cryptic
package crypto

object Caesar {
  case class Key(offset: Int) {
    require(offset != 0)
  }
  implicit def encrypt(implicit key: Key): Encrypt = (plainText: PlainText) =>
    CipherText(plainText.map(b ⇒ (b + key.offset).toByte))
  implicit def decrypt(implicit key: Key): Decrypt = (cipherText: CipherText) =>
    Right[String, PlainText](PlainText(cipherText.map(b => (b - key.offset).toByte)))
}
