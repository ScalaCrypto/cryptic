package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.{Success, Try}

class RsaAesSpec extends AnyFlatSpec with Matchers:
  import cryptic.codec.default.given

  import RsaAes.{*, given}
  given f:Functor[Try] = Functor.tryFunctor

  val keyPair: KeyPair = Rsa.newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  val text: String = "secret" * 10000 // Large data
  "RsaAes" should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    val encrypted = text.encrypted

    given privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "RsaAes" should "hide plaintext" in:
    new String(text.encrypted.bytes.get.mutable)
      .contains(text.getBytes()) shouldBe false
