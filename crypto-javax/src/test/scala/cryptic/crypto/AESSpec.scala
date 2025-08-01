package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AESSpec extends AnyFlatSpec with Matchers:
  import AES.{given, *}
  given params: AESParams = AESParams()
  given password: AESPassphrase = AESPassphrase("secret")
  val text = "nisse"

  "AES" should "support encryption and decryption" in:
    val encrypted = text.encrypted

    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "AES" should "hide plaintext" in:
    new String(text.encrypted.bytes)
      .contains(text.getBytes()) shouldBe false

  "AESParams keyspecLength" should
    "only allow 126, 192, 256" in:
      AESParams(keyspecLength = 192) should not be null
  "AESParams keyspecLength" should
    "not allow 127" in:
      val exception = intercept[AssertionError]:
        AESParams(keyspecLength = 127)
      exception.getMessage shouldBe "assertion failed: Invalid keyspecLength: 127. Allowed values are 128, 192, 256."
