package cryptic
package crypto

import cryptic.support.{AsyncTestBase, TestBase}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class ReverseSpec extends AsyncTestBase:
  import cryptic.codec.default.{*, given}
  import Reverse.{*, given}

  private val text = "nisse"
  private val encrypted: Encrypted[String] = text.encrypted.futureValue

  "Reverse Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted.futureValue shouldEqual text

  "Reverse Encrypted" should "hide plaintext" in:
    new String(encrypted.bytes.mutable)
      .contains(text) shouldBe false

  "Reverse" should "be reversed" in:
    encrypted.bytes.mutable shouldEqual text.reverse.getBytes
