package cryptic
package crypto

import cryptic.support.AsyncTestBase
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AesSpec extends AsyncTestBase:
  import cryptic.codec.default.given
  import Aes.{*, given}
  given password: Passphrase = Passphrase("secret")
  val text = "nisse"

  "Aes" should "support encryption and decryption" in:
    val encrypted = text.encrypted.futureValue
    encrypted.decrypted.futureValue shouldEqual text

  "Aes" should "hide plaintext" in:
    text.encrypted.map { enc =>
      new String(enc.bytes.mutable)
        .contains(text.getBytes()) shouldBe false
    }
