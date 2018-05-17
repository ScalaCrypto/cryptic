package cryptic
package crypto
  import javax.crypto.Cipher

import java.security.{PrivateKey, PublicKey}

object RSA {

  val cipher: Cipher = Cipher.getInstance("RSA")

  implicit def encrypt(implicit key: PublicKey): Encrypt = (plainText: PlainText) => {
    cipher.init(Cipher.ENCRYPT_MODE, key)
    CipherText(hash(plainText))(cipher.doFinal(plainText))
  }
  
  implicit def decrypt(implicit key: PrivateKey): Decrypt = (cipherText: CipherText) => {
    cipher.init(Cipher.DECRYPT_MODE, key)
    Right[String, PlainText](PlainText(cipher.doFinal(cipherText.bytes)))
  }
}
