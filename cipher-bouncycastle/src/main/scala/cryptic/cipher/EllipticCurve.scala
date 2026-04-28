package cryptic
package cipher

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.IESParameterSpec
import org.bouncycastle.util.encoders.Hex

import java.security.spec.ECGenParameterSpec
import java.security.{Key, KeyPair, KeyPairGenerator, Security, Signature}
import javax.crypto.Cipher
import scala.util.{Success, Try}

/** Provides an implementation of elliptic curve cryptography (ECC) for
  * performing cryptographic operations such as key pair generation, digital
  * signature creation and validation, and encryption and decryption using the
  * ECIES (Elliptic Curve Integrated Encryption Scheme) algorithm.
  *
  * This object utilizes the Bouncy Castle Provider for cryptographic operations
  * and adheres to the `Asymmetric` and `Signer` typeclasses, allowing it to
  * perform operations in an effectful manner within the `Try` effect wrapper.
  *
  * Methods:
  *   - newCipher: Creates an ECIES cipher instance.
  *   - newKeyPair: Generates an EC key pair using the secp256r1 curve.
  *   - newSignature: Creates a SHA256withECDSA signature instance.
  */
 object EllipticCurve extends Asymmetric[Try] with Signer[Try]:
  val version: Version = FixedVersion(0, 0, 0, 1)
  Security.addProvider(new BouncyCastleProvider())
  private val generator: KeyPairGenerator = KeyPairGenerator.getInstance("EC")
  generator.initialize(new ECGenParameterSpec("secp256r1"))
  given functor: Functor[Try] = Functor.tryFunctor

  object default:
    export cryptic.default.{given, *}
    export EllipticCurve.{given, *}

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

  def newKeyPair(): KeyPair = generator.generateKeyPair()

  def newSignature: Try[Signature] = Success(
    Signature.getInstance("SHA256withECDSA", "BC")
  )
