package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AesCbcSpec extends AnyFlatSpec with Matchers:
  import cryptic.codec.default.given
  import Aes.{given, *}
  given params: AesParams = CbcParams()
  given password: AesPassphrase = AesPassphrase("secret")
  val text = "nisse"

  "AesCbc" should "support encryption and decryption" in:
    val encrypted = text.encrypted
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "AesCbc" should "hide plaintext" in:
    new String(text.encrypted.bytes.mutable)
      .contains(text.getBytes()) shouldBe false

  "AesCbcParams keyspecLength" should
    "only allow 126, 192, 256" in:
      CbcParams(keyspecLength = 192) should not be null
  "AesCbcParams keyspecLength" should
    "not allow 127" in:
      val exception = intercept[IllegalArgumentException]:
        CbcParams(keyspecLength = 127)
      exception.getMessage shouldBe "requirement failed: Invalid keyspecLength: 127. Allowed values are 128, 192, 256."
