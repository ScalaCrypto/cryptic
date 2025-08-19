package cryptic
package crypto

import java.security.{Key, KeyPair, KeyPairGenerator, PrivateKey, PublicKey}
import java.security.spec.MGF1ParameterSpec
import javax.crypto.Cipher
import javax.crypto.spec.{OAEPParameterSpec, PSource}

/** The Rsa object provides encryption, decryption, and key generation
 * functionalities using the RSA algorithm. This object extends the
 * Asymmetric trait and implements methods to create a cipher and generate
 * RSA key pairs.
 *
 * This object uses RSA/ECB/OAEPWithSHA-256AndMGF1Padding as the cipher
 * transformation, ensuring modern and secure default configurations for
 * encryption and decryption processes.
 *
 * Methods:
 *
 *   - newCipher: Creates a new RSA cipher instance configured with the 
 *     specified mode (encrypt or decrypt) and key.
 *   - newKeyPair: Generates a new RSA key pair with the specified key size.
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

  def newKeyPair(size: Int): KeyPair =
    val generator = KeyPairGenerator.getInstance("RSA")
    generator.initialize(size)
    generator.generateKeyPair()
