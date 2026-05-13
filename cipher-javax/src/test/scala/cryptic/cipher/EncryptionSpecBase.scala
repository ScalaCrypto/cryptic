package cryptic
package cipher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.compiletime.deferred
import scala.util.{Failure, Success, Try}

trait EncryptionSpecBase extends AsymmetricSpecBase:
  given encrypt: Encrypt[Try] = deferred
  given decrypt: Decrypt[Try] = deferred

  "Asymmetric encryption" should "support encryption and decryption" in:
    val encrypted = text.encrypted
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x               => fail(s"does not decrypt: $x")

  it should "not support AAD" in:
    val encrypted = text.encrypted("AAD".aad)
    intercept[UnsupportedOperationException]:
      encrypted.bytes.get

  it should "hide plaintext" in:
    new String(text.encrypted.bytes.get.mutable)
      .contains(text.getBytes()) shouldBe false

  it should "fail to encode AAD in the CipherText" in:
    import cryptic.Functor.tryFunctor
    val aad = "AAD".aad
    text
      .encrypted(aad)
      .cipherText
      .failure

  it should "reject wrong encryption version" in:
    val tampered = text.encrypted.splitWith:
      case IArray(_, bytes) =>
        val wrongVersion = FixedVersion(0, 0, 0, 0).bytes
        Success(CipherText(wrongVersion, bytes))

    Encrypted[Try, String](tampered).decrypted match
      case Failure(e) =>
        e shouldBe a[IllegalArgumentException]
        e.getMessage should include("Unsupported version")
      case x =>
        fail(s"Expected decryption to fail due to version mismatch, got $x")
