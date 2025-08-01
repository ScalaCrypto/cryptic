package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class CaesarSpec extends AnyFlatSpec with Matchers:
  import Caesar.*
  import Caesar.given
  implicit val key1: Key = Caesar.Key(1)

  private val text = "nisse"
  private val encrypted = Encrypted(text)
  "Caesar Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted match
      case Success(decrypted) ⇒ decrypted shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "Caesar Encrypted" should "hide plaintext" in:
    new String(encrypted.cipherText.bytes)
      .contains(text) shouldBe false

  "Caesar key zero" should "not be valid" in:
    assertThrows[IllegalArgumentException] { Caesar.Key(0) }
 
  "Caesar Encrypted" should "be rotated" in:
    encrypted.bytes shouldEqual "ojttf".getBytes