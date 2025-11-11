package cryptic
package cipher
package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class CaesarSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.codec.default.given
  import Caesar.{*, given}
  import cryptic.Functor.tryFunctor
  given key1: Key = Caesar.Key(1)

  private val text = "nisse"
  private val encrypted: Encrypted[Try, String] = text.encrypted
  "Caesar Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted.success.value shouldEqual text

  "Caesar Encrypted" should "hide plaintext" in:
    encrypted.bytes.map(b =>
      new String(b.mutable).contains(text)
    ) shouldBe Success(false)

  "Caesar key zero" should "not be valid" in:
    given key0:Key = Key(0)
    text.encrypted.bytes.failure

  "Caesar Encrypted" should "be rotated" in:
    encrypted.bytes.success.value.toSeq shouldEqual "ojttf".getBytes
