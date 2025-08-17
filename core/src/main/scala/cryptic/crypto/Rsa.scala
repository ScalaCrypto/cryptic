package cryptic
package crypto

import java.security.{Key, KeyPair, KeyPairGenerator, PrivateKey, PublicKey}
import java.security.spec.MGF1ParameterSpec
import javax.crypto.Cipher
import javax.crypto.spec.{OAEPParameterSpec, PSource}

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

  def newCipher(mode: Int, key: Key): Cipher =
    val oaepParams: OAEPParameterSpec = new OAEPParameterSpec(
      "SHA-256",
      "MGF1",
      MGF1ParameterSpec.SHA256,
      PSource.PSpecified.DEFAULT
    )
    val cipher = Cipher.getInstance("RSA/ECB/OAEPWithSHA-256AndMGF1Padding")
    cipher.init(mode, key, oaepParams)
    cipher

  def newKeyPair(size:Int): KeyPair = {
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(size)
    generator.generateKeyPair()
  }
