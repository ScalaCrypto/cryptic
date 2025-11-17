package cryptic
package cipher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}

class AesSpec extends AnyFlatSpec with Matchers:
  import Aes.default.{given, *}
  given password: Passphrase = Passphrase("secret")
  val text = "nisse"

  it should "support encryption and decryption" in:
    val encrypted: Encrypted[Try, String] = text.encrypted
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  it should "hide plaintext" in:
    val enc: Encrypted[Try, String] = text.encrypted
    new String(enc.bytes.get.mutable)
      .contains(text.getBytes()) shouldBe false

  it should "encode AAD" in:
    import cryptic.Functor.tryFunctor
    val expected = "AAD".getBytes.aad
    text
      .encrypted(expected)
      .splitWith:
        case IArray(version, actual, salt, iv, bytes) =>
          tryFunctor.pure(actual shouldBe expected)

  it should "detect if AAD is altered" in:
    import cryptic.Functor.tryFunctor
    val aad = "AAD".aad
    val tampered = text
      .encrypted(aad)
      .splitWith:
        case IArray(version, aad2, salt, iv, bytes) =>
          tryFunctor.pure(
            CipherText(version, "tampered".getBytes.aad, salt, iv, bytes)
          )
    Encrypted[Try, String](tampered).decrypted match
      case Failure(e) => e.getMessage should include("Tag mismatch")
      case _ => fail("Expected decryption to fail due to AAD tampering")

  it should "reject wrong version" in:
    import cryptic.Functor.tryFunctor
    val tampered = text.encrypted.splitWith:
      case IArray(_, aad, salt, iv, bytes) =>
        val wrongVersion = FixedVersion(0, 0, 0, 0).bytes
        tryFunctor.pure(CipherText(wrongVersion, aad, salt, iv, bytes))
    Encrypted[Try, String](tampered).decrypted match
      case Failure(e) =>
        e shouldBe a[IllegalArgumentException]
        e.getMessage should include("Unsupported version")
      case _ => fail("Expected decryption to fail due to version mismatch")
