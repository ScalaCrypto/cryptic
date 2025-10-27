package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import java.security.{KeyPair, PrivateKey, PublicKey}
import javax.crypto.IllegalBlockSizeException
import scala.util.{Failure, Success, Try}

class RsaSpec extends AnyFlatSpec with Matchers with TryValues:
  import cryptic.codec.default.given
  import Rsa.{*, given}
  val keyPair: KeyPair = Rsa.newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  val text = "secret"

  "RSA" should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    val encrypted = text.encrypted

    given privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "RSA" should "not support AAD" in:
    val encrypted = text.encrypted("AAD".aad)
    intercept[UnsupportedOperationException]:
      encrypted.bytes.get

  "RSA" should "fail on large data" in:
    ("secret" * 1000).encrypted.cipherText.failure.exception shouldBe a[
      IllegalBlockSizeException
    ]

  "RSA" should "hide plaintext" in:
    new String(text.encrypted.bytes.get.mutable)
      .contains(text.getBytes()) shouldBe false

  "RSA" should "fail to encode AAD unauthenticated in the CipherText" in:
    import cryptic.Functor.tryFunctor
    val expected = "AAD".aad
    text
      .encrypted(expected)
      .cipherText
      .failure
