package cryptic

object Caesar {
  case class Key(offset:Int) {
    require(offset != 0)
  }
  implicit def encryptor(key: Key): Encryptor = (plainText: PlainText) => CipherText(plainText.map(b ⇒ (b + key.offset).toByte))
  implicit def decryptor(key: Key): Decryptor = (cipherText: CipherText) => Right[String, PlainText](PlainText(cipherText.map(b => (b - key.offset).toByte)))
}