package cryptic
package crypto

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}
import javax.crypto.Cipher
import scala.util.Try

/** RSA object provides encryption, decryption, and key generation
  * functionalities using the RSA algorithm.
  *
  * @define encrypt
  *   Performs RSA encryption on the given plain text using the provided public
  *   key.
  * @define decrypt
  *   Performs RSA decryption on the given cipher text using the provided
  *   private key.
  * @define keygen
  *   Generates an RSA key pair with the specified size.
  */
object Rsa:
  val cipher: Cipher = Cipher.getInstance("RSA")
  given encrypt(using key: PublicKey): Encrypt =
    (plainText: PlainText) =>
      cipher.init(Cipher.ENCRYPT_MODE, key)
      CipherText(plainText.manifest, cipher.doFinal(plainText.bytes))

  given decrypt(using key: PrivateKey): Decrypt =
    (cipherText: CipherText) =>
      val Array(manifest, bytes) = cipherText.split
      cipher.init(Cipher.DECRYPT_MODE, key)
      Try[PlainText](PlainText(cipher.doFinal(bytes), manifest))

  /** Generates a new RSA key pair with the specified key size.
    *
    * @param size
    *   the size of the keys to generate, in bits
    * @return
    *   a new KeyPair instance containing the generated public and private keys
    */
  def keygen(size: Int): KeyPair =
    val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
    keyPairGenerator.initialize(size)
    keyPairGenerator.genKeyPair
