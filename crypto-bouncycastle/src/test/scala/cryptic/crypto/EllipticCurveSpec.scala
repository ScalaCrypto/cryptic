package cryptic
package crypto

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.Success

class EllipticCurveSpec extends AnyFlatSpec with Matchers:
  import cryptic.codec.default.given
  import EllipticCurve.{*, given}
  private val keyPair: KeyPair = newKeyPair()
  given publicKey: PublicKey = keyPair.getPublic
  private val text = "secret"

  "EllipticCurve" should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    val encrypted: Encrypted[String] = text.encrypted

    given privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "EllipticCurve" should "handle large data" in:
    given privateKey: PrivateKey = keyPair.getPrivate
    val large = "secret" * 1000
    val enc = large.encrypted
    enc.decrypted shouldBe Success(large)

  "EllipticCurve" should "hide the plain text" in:
    new String(text.encrypted.bytes.mutable)
      .contains(text.getBytes()) shouldBe false
