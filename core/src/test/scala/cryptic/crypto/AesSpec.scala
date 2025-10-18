package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Success, Try}

class AesSpec extends AnyFlatSpec with Matchers:
  import cryptic.codec.default.given
  import Aes.{*, given}
  given password: Passphrase = Passphrase("secret")
  val text = "nisse"

  "Aes" should "support encryption and decryption" in:
    val encrypted: Encrypted[Try, String] = text.encrypted
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "Aes" should "hide plaintext" in:
    val enc: Encrypted[Try, String] = text.encrypted
    new String(enc.bytes.get.mutable)
      .contains(text.getBytes()) shouldBe false
