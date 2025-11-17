package cryptic
package cipher

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import java.security.{KeyPair, PrivateKey, PublicKey}
import javax.crypto.IllegalBlockSizeException
import scala.util.{Failure, Success, Try}

class RsaSpec extends AnyFlatSpec with Matchers with TryValues:
  import Rsa.default.{*, given}
  val keyPair: KeyPair = Rsa.newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  val text = "secret"

  it should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    val encrypted = text.encrypted

    given privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x               => fail(s"does not decrypt: $x")

  it should "not support AAD" in:
    val encrypted = text.encrypted("AAD".aad)
    intercept[UnsupportedOperationException]:
      encrypted.bytes.get

  it should "fail on large data" in:
    ("secret" * 1000).encrypted.cipherText.failure.exception shouldBe a[
      IllegalBlockSizeException
    ]

  it should "hide plaintext" in:
    new String(text.encrypted.bytes.get.mutable)
      .contains(text.getBytes()) shouldBe false

  it should "fail to encode AAD unauthenticated in the CipherText" in:
    import cryptic.Functor.tryFunctor
    val expected = "AAD".aad
    text
      .encrypted(expected)
      .cipherText
      .failure

  it should "reject wrong version" in:
    import cryptic.Functor.tryFunctor
    val tampered = text.encrypted.splitWith:
      case IArray(_, bytes) =>
        val wrongVersion = FixedVersion(0, 0, 0, 0).bytes
        tryFunctor.pure(CipherText(wrongVersion, bytes))

    given privateKey: PrivateKey = keyPair.getPrivate

    Encrypted[Try, String](tampered).decrypted match
      case Failure(e) =>
        e shouldBe a[IllegalArgumentException]
        e.getMessage should include("Unsupported version")
      case x =>
        fail(s"Expected decryption to fail due to version mismatch, got $x")
