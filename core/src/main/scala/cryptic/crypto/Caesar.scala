package cryptic
package crypto

object Caesar {
  case class Key(offset: Int) {
    require(offset != 0)
  }
  implicit def encrypt(implicit key: Key): Encrypt = (plainText: PlainText) => {
    val bytes = plainText.map(b ⇒ (b + key.offset).toByte)
    CipherText(bytes)
  }
  implicit def decrypt(implicit key: Key): Decrypt = (cipherText: CipherText) =>
    Right[String, PlainText](PlainText(cipherText.bytes.map(b => (b - key.offset).toByte)))

  def keygen(offset:Int): Key = Key(offset)
}
