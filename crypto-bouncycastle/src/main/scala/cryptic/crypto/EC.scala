package cryptic
package crypto

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import org.bouncycastle.jce.provider.BouncyCastleProvider

import java.security.*
import javax.crypto.Cipher
import scala.util.Try

/** Elliptic Curve Integrated Encryption Scheme depends on private and public
  * keys. The public key should be given for encryption and the private key for
  * decryption
  */
object EC:
  Security.addProvider(new BouncyCastleProvider())
  val cipher: Cipher = Cipher.getInstance("ECIES", "BC")
  given encrypt(using key: PublicKey): Encrypt =
    (plainText: PlainText) =>
      cipher.init(Cipher.ENCRYPT_MODE, key)
      CipherText(cipher.doFinal(plainText))

  given decrypt(using key: PrivateKey): Decrypt =
    (cipherText: CipherText) =>
      cipher.init(Cipher.DECRYPT_MODE, key)
      Try[PlainText](PlainText(cipher.doFinal(cipherText.bytes)))

  /** Generates a new elliptic curve key pair.
    *
    * @param size
    *   the size of the key to generate
    * @return
    *   a newly generated KeyPair
    */
  def keygen(size: Int): KeyPair =
    val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("EC")
    keyPairGenerator.initialize(size)
    keyPairGenerator.genKeyPair
