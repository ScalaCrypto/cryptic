package cryptic
package crypto

import javax.crypto.{Cipher, SecretKey}

object AES {
  def apply() : AES = new AES("CBC/PKCS5Padding")
}
  /**
    *
    * @param transformation without leading AES
    * @return
    */
class AES(transformation:String) {
  val cipher: Cipher = Cipher.getInstance(s"AES/$transformation")

  implicit def encrypt(implicit key: SecretKey): Encrypt = (plainText: PlainText) => {
    cipher.init(Cipher.ENCRYPT_MODE, key)
    CipherText(hash(plainText))(cipher.doFinal(plainText))
  }

  implicit def decrypt(implicit key: SecretKey): Decrypt = (cipherText: CipherText) => {
    import javax.crypto.spec.IvParameterSpec
//    val encoded = key.getEncoded
//    val ivParameterSpec = new IvParameterSpec(encoded)
    cipher.init(Cipher.DECRYPT_MODE, key)
    Right[String, PlainText](PlainText(cipher.doFinal(cipherText.bytes)))
  }

  def keygen(size: Int): SecretKey = {
    import javax.crypto.KeyGenerator
    val kgen = KeyGenerator.getInstance("AES")
    kgen.init(size)
    kgen.generateKey
  }
}
