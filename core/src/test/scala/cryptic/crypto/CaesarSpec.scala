package cryptic
package crypto

import cryptic.support.{AsyncTestBase, TestBase}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class CaesarSpec extends AsyncTestBase:
  import cryptic.codec.default.given
  import Caesar.*
  import Caesar.given
  given key1: Key = Caesar.Key(1)

  private val text = "nisse"
  private val encrypted = text.encrypted.futureValue
  "Caesar Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted.futureValue shouldEqual text

  "Caesar Encrypted" should "hide plaintext" in:
    new String(encrypted.bytes.mutable)
      .contains(text) shouldBe false

  "Caesar key zero" should "not be valid" in:
    assertThrows[IllegalArgumentException]:
      Caesar.Key(0)

  "Caesar Encrypted" should "be rotated" in:
    encrypted.bytes.mutable shouldEqual "ojttf".getBytes
