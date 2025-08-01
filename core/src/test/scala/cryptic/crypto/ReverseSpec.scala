package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class ReverseSpec extends AnyFlatSpec with Matchers:
  import Reverse.{*, given}

  private val text = "nisse"
  private val encrypted: Encrypted[String] = Encrypted(text)
  
  "Reverse Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted match
      case Success(decrypted) ⇒ decrypted shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "Reverse Encrypted" should "hide plaintext" in:
    new String(encrypted.cipherText.bytes)
      .contains(text) shouldBe false
    
  "Reverse" should "be reversed" in:
    encrypted.bytes shouldEqual text.reverse.getBytes
