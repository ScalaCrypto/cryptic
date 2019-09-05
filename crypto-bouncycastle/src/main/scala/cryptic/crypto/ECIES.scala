package cryptic.crypto

import java.security._

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import javax.crypto.Cipher
import org.bouncycastle.jce.provider.BouncyCastleProvider

/**
  * RSA encryption depends  private and public keys.
  * The public key should be implicitly available for encryption and
  * the private key for decryption
  */
object ECIES {
  Security.addProvider(new BouncyCastleProvider())
  val cipher: Cipher = Cipher.getInstance("ECIES", "BC")
  implicit def encrypt(implicit key: PublicKey): Encrypt = (plainText: PlainText) => {
    cipher.init(Cipher.ENCRYPT_MODE, key)
    CipherText(cipher.doFinal(plainText))
  }

  implicit def decrypt(implicit key: PrivateKey): Decrypt = (cipherText: CipherText) => {
    cipher.init(Cipher.DECRYPT_MODE, key)
    Right[String, PlainText](PlainText(cipher.doFinal(cipherText.bytes)))
  }

  def keygen(size: Int): KeyPair = {
    val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("EC")
    keyPairGenerator.initialize(size)
    keyPairGenerator.genKeyPair
  }
}
