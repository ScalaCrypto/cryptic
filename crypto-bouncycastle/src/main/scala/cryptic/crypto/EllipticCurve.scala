package cryptic
package crypto

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.IESParameterSpec
import org.bouncycastle.util.encoders.Hex

import java.security.*
import java.security.spec.ECGenParameterSpec
import javax.crypto.Cipher
import scala.util.{Success, Try}

/** Elliptic Curve (ECIES) asymmetric crypto built on Bouncy Castle.
  *
  * Provides `Encrypt[Try]`/`Decrypt[Try]` via the shared `Asymmetric`
  * utilities, using the BC provider and a pre-configured `IESParameterSpec` for
  * ECIES.
  *
  * Notes:
  *   - Requires a public key for encryption and a private key for decryption to
  *     be in scope.
  *   - Uses the Bouncy Castle provider (added at static initialization) and
  *     `ECIES` cipher.
  *   - The current setup initializes the key pair generator for `secp256r1`.
  *   - The `AAD` is carried alongside the payload bytes inside `CipherText` and
  *     restored into `PlainText` during decryption; ECIES itself does not
  *     interpret the AAD.
  */
object EllipticCurve extends Asymmetric[Try]:
  val version: Version = FixedVersion(0, 0, 0, 1)
  Security.addProvider(new BouncyCastleProvider())
  private val generator: KeyPairGenerator = KeyPairGenerator.getInstance("EC")
  generator.initialize(new ECGenParameterSpec("secp256r1"))

  def newKeyPair(): KeyPair = generator.generateKeyPair()

  override def newCipher(mode: Int, key: Key): Cipher =
    val derivation = Hex.decode("00112233445566778899AABBCCDDEEFF")
    val encoding = Hex.decode("112233445566778899AABBCCDDEEFF00")
    val macKeySize = 128
    val cipherKeySize = 128
    val nonce = secureRandom.newBytes(16)
    val iesParams =
      new IESParameterSpec(
        derivation,
        encoding,
        macKeySize,
        cipherKeySize,
        nonce.mutable
      )

    val cipher = Cipher.getInstance("ECIES", "BC")
    cipher.init(mode, key, iesParams)
    cipher

  override def encodeCipherText(encrypted: IArray[Byte]): CipherText =
    CipherText(version.bytes, encrypted)

  override def decodeCipherText
      : PartialFunction[IArray[IArray[Byte]], Try[IArray[Byte]]] =
    case IArray(ver, encrypted) if version.supports(ver) => Success(encrypted)
    case IArray(ver, _)                                  => version.failed(ver)
