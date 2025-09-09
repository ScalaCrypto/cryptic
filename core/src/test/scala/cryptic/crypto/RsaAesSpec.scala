package cryptic
package crypto

import cryptic.support.AsyncTestBase
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.Success

class RsaAesSpec extends AsyncTestBase:
  import cryptic.codec.default.given

  import RsaAes.{*, given}

  val keyPair: KeyPair = Rsa.newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  val text: String = "secret" * 10000 // Large data
  // Note no need for the private key when encrypting
  val encrypted: Encrypted[String] = text.encrypted.futureValue

  "RsaAes" should "support encryption and decryption" in:

    given privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted.futureValue shouldEqual text

  "RsaAes" should "hide plaintext" in:
    new String(encrypted.bytes.mutable)
      .contains(text.getBytes()) shouldBe false
