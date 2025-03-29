package cryptic
package crypto

import cryptic.serialization.StringSerializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class CaesarSpec extends AnyFlatSpec with Matchers:
  import Caesar.*
  import Caesar.given
  import StringSerializer.*
  implicit val key1: Key = Caesar.Key(1)

  "Caesar Encrypted" should "support encryption and decryption" in:
    Encrypted("nisse").decrypted match
      case Success(decrypted) ⇒ decrypted shouldEqual "nisse"
      case x ⇒ fail(s"does not decrypt: $x")

  "Caesar Encrypted" should "hide plaintext" in:
    new String(Encrypted("nisse").cipherText.bytes)
      .contains("nisse") shouldBe false

  "Caesar zero" should "not be valid" in:
    assertThrows[IllegalArgumentException] { Caesar.Key(0) }
