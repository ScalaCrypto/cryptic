package cryptic
package cipher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.{Failure, Success, Try}
import cryptic.FixedVersion

class RsaAesSpec extends AnyFlatSpec with Matchers:
  import RsaAes.default.{given, *}

  val keyPair: KeyPair = Rsa.newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  val text: String = "secret" * 10000 // Large data
  it should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    val encrypted = text.encrypted
    given privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  it should "fail with mismatched versions" in:
    val tampered = text.encrypted.splitWith:
      case IArray(_, aad, iv, key, text) =>
        val wrongVersion = FixedVersion(0, 0, 0, 0).bytes
        cryptic.Functor.tryFunctor.pure(
          CipherText(wrongVersion, aad, iv, key, text)
        )
    given privateKey: PrivateKey = keyPair.getPrivate
    Encrypted[Try, String](tampered).decrypted match
      case Failure(e) =>
        e shouldBe a[IllegalArgumentException]
        e.getMessage should include("Unsupported version")
      case _ => fail("Expected decryption to fail due to version mismatch")

  it should "hide plaintext" in:
    new String(text.encrypted.bytes.get.mutable)
      .contains(text.getBytes()) shouldBe false
