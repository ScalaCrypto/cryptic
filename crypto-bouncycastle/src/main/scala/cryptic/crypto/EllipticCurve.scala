package cryptic
package crypto

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.IESParameterSpec
import org.bouncycastle.util.encoders.Hex

import java.security.*
import java.security.spec.ECGenParameterSpec
import javax.crypto.Cipher
import scala.util.Try

/** Elliptic Curve (ECIES) asymmetric crypto built on Bouncy Castle.
  *
  * Provides `Encrypt[Try]`/`Decrypt[Try]` via the shared `Asymmetric` utilities, using
  * the BC provider and a pre-configured `IESParameterSpec` for ECIES.
  *
  * Notes:
  * - Requires a public key for encryption and a private key for decryption to be in scope.
  * - Uses the Bouncy Castle provider (added at static initialization) and `ECIES` cipher.
  * - The current setup initializes the key pair generator for `secp256r1`.
  * - The `AAD` is carried alongside the payload bytes inside `CipherText` and restored
  *   into `PlainText` during decryption; ECIES itself does not interpret the AAD.
  */
object EllipticCurve extends Asymmetric[Try]:
  Security.addProvider(new BouncyCastleProvider())
  private val secureRandom = new SecureRandom()
  private val generator: KeyPairGenerator = KeyPairGenerator.getInstance("EC")
  generator.initialize(new ECGenParameterSpec("secp256r1"))

  def newKeyPair(): KeyPair = generator.generateKeyPair()

  override def newCipher(mode: Int, key: Key): Cipher =
    val derivation = Hex.decode("00112233445566778899AABBCCDDEEFF")
    val encoding = Hex.decode("112233445566778899AABBCCDDEEFF00")
    val macKeySize = 128
    val cipherKeySize = 128
    val nonce = new Array[Byte](16)
    secureRandom.nextBytes(nonce)

    val iesParams =
      new IESParameterSpec(
        derivation,
        encoding,
        macKeySize,
        cipherKeySize,
        nonce
      )

    val cipher = Cipher.getInstance("ECIES", "BC")
    cipher.init(mode, key, iesParams)
    cipher
