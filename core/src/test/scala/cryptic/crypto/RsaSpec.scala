package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.Success

class RsaSpec extends AnyFlatSpec with Matchers:
  import cryptic.codec.default.given
  import Rsa.{*, given}
  val keyPair: KeyPair = keygen(2048)
  given publicKey: PublicKey = keyPair.getPublic
  val text = "nisse"

  "RSA" should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    val encrypted = text.encrypted

    given privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "RSA" should "hide plaintext" in:
    new String(text.encrypted.bytes.mutable)
      .contains(text.getBytes()) shouldBe false
