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
object Rsa extends Asymmetric:

  export java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

  private val algorithmName = "RSA"

  def newCipher(): Cipher = Cipher.getInstance(algorithmName)

  def newKeyPairGenerator(): KeyPairGenerator =
    KeyPairGenerator.getInstance(algorithmName)
