package cryptic
package crypto

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}
import javax.crypto.Cipher

/**
 * RSA encryption depends private and public keys. The public key should be implicitly available for encryption and the private key for decryption
 */
object RSA {
  val cipher: Cipher = Cipher.getInstance("RSA")
  implicit def encrypt(implicit key: PublicKey): Encrypt = (plainText: PlainText) => {
    cipher.init(Cipher.ENCRYPT_MODE, key)
    CipherText(cipher.doFinal(plainText))
  }

  implicit def decrypt(implicit key: PrivateKey): Decrypt = (cipherText: CipherText) => {
    cipher.init(Cipher.DECRYPT_MODE, key)
    Right[String, PlainText](PlainText(cipher.doFinal(cipherText.bytes)))
  }

  def keygen(size: Int): KeyPair = {
    val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
    keyPairGenerator.initialize(size)
    keyPairGenerator.genKeyPair
  }
}
