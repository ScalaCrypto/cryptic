package cryptic
package crypto
package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class ReverseSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.codec.default.{*, given}
  import Reverse.{*, given}

  private val text = "nisse"
  private val encrypted: Encrypted[Try, String] = text.encrypted

  "Reverse Encrypted" should "support encryption and decryption" in:
    val decrypted: Try[String] = encrypted.decrypted
    decrypted shouldEqual Success(text)

  "Reverse Encrypted" should "hide plaintext" in:
    encrypted.bytes.map(b =>
      new String(b.mutable).contains(text)
    ) shouldBe Success(false)

  "Reverse" should "be reversed" in:
    encrypted.bytes.success.value.toSeq shouldEqual text.reverse.getBytes
