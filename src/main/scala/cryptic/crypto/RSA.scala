package cryptic
package crypto
  import javax.crypto.Cipher

import java.security.{PrivateKey, PublicKey}

object RSA{

  val cipher: Cipher = Cipher.getInstance("RSA")

  implicit def encrypt(implicit privateKey: PublicKey): Encrypt = (plainText: PlainText) => {
    cipher.init(Cipher.ENCRYPT_MODE, privateKey)
    CipherText(cipher.doFinal(plainText))
  }
  
  implicit def decrypt(implicit publicKey: PrivateKey): Decrypt = (cipherText: CipherText) => {
    cipher.init(Cipher.DECRYPT_MODE, publicKey)
    Right[String, PlainText](PlainText(cipher.doFinal(cipherText)))
  }
}
