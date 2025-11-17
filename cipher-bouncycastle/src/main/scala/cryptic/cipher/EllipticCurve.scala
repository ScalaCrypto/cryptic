package cryptic
package cipher

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import org.bouncycastle.jce.provider.BouncyCastleProvider
import org.bouncycastle.jce.spec.IESParameterSpec
import org.bouncycastle.util.encoders.Hex

import java.security.*
import java.security.spec.ECGenParameterSpec
import javax.crypto.Cipher
import scala.util.{Success, Try}

/** Provides cryptographic operations using Elliptic Curve (EC) cryptography.
  *
  * This object serves as a concrete implementation of the `Asymmetric` trait
  * specialized for cryptographic operations based on Elliptic Curve (EC)
  * cryptography. It supports key pair generation, encryption, and decryption
  * using ECIES (Elliptic Curve Integrated Encryption Scheme).
  *
  * The implementation uses the BouncyCastle library as the cryptographic
  * provider.
  */
object EllipticCurve extends Asymmetric[Try]:
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
