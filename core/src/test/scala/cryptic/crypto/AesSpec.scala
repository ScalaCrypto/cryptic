package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class AesSpec extends AnyFlatSpec with Matchers:
  import cryptic.codec.default.given
  import Aes.{*, given}
  given password: Passphrase = Passphrase("secret")
  val text = "nisse"

  "Aes" should "support encryption and decryption" in:
    val encrypted: Encrypted[Try, String] = text.encrypted
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "Aes" should "hide plaintext" in:
    val enc: Encrypted[Try, String] = text.encrypted
    new String(enc.bytes.get.mutable)
      .contains(text.getBytes()) shouldBe false

  "Aes" should "encode AAD" in:
    import cryptic.Functor.tryFunctor
    val expected = "AAD".getBytes.aad
    text
      .encrypted(expected)
      .cipherText
      .map: cipherText =>
        val IArray(actual, salt, iv, bytes) = cipherText.split
        actual shouldBe expected

  "Aes" should "detect if AAD is altered" in:
    import cryptic.Functor.tryFunctor
    val aad = "AAD".aad
    val tampered = text
      .encrypted(aad)
      .cipherText
      .map: cipherText =>
        val IArray(aad2, salt, iv, bytes) = cipherText.split
        CipherText("tampered".getBytes.aad, salt, iv, bytes)
    Encrypted[Try, String](tampered).decrypted match
      case Failure(e) => e.getMessage should include("Tag mismatch")
      case _ => fail("Expected decryption to fail due to AAD tampering")
