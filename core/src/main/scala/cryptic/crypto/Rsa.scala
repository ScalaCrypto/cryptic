package cryptic
package crypto

import java.security.{Key, KeyPair, KeyPairGenerator, PrivateKey, PublicKey}
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
  export java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

  def newCipher(opMode:Int, key:Key): Cipher = {
    val cipher = Cipher.getInstance("RSA")
    cipher.init(opMode, key)
    cipher
  }

  given encrypt(using key: PublicKey): Encrypt =
    (plainText: PlainText) =>
      val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
      CipherText(
        plainText.manifest,
        cipher.doFinal(plainText.bytes.mutable).immutable
      )

  given decrypt(using key: PrivateKey): Decrypt =
    (cipherText: CipherText) =>
      val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, key)
      val IArray(manifest, bytes) = cipherText.split
      Try[PlainText](
        PlainText(cipher.doFinal(bytes.mutable).immutable, manifest)
      )

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
