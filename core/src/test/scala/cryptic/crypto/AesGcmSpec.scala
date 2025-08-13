package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AesGcmSpec extends AnyFlatSpec with Matchers:
  import cryptic.codec.default.given
  import Aes.{*, given}
  given params: AesParams = GcmParams()
  given password: AesPassphrase = AesPassphrase("secret")
  val text = "nisse"

  "AesGcm" should "support encryption and decryption" in:
    val encrypted = text.encrypted
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "AesGcm" should "hide plaintext" in:
    new String(text.encrypted.bytes.mutable)
      .contains(text.getBytes()) shouldBe false

  "GcmParams keyspecLength" should
    "only allow more than 256" in:
      GcmParams(keyspecLength = 512) should not be null
  "GcmParams keyspecLength" should
    "not allow 192" in:
      val exception = intercept[IllegalArgumentException]:
        GcmParams(keyspecLength = 192)
      exception.getMessage shouldBe "requirement failed: Key length must be at least 256 bits"
