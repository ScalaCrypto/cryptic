package cryptic
package crypto

import cryptic.support.AsyncTestBase
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import javax.crypto.IllegalBlockSizeException
import scala.util.Success

class RsaSpec extends AsyncTestBase:
  import cryptic.codec.default.given
  import Rsa.{*, given}
  val keyPair: KeyPair = Rsa.newKeyPair(2048)
  given publicKey: PublicKey = keyPair.getPublic
  val text = "secret"
  // Note no need for the private key when encrypting
  val enc: Encrypted[String] = text.encrypted.futureValue

  "RSA" should "support encryption and decryption" in:
    given privateKey: PrivateKey = keyPair.getPrivate
    enc.decrypted.futureValue shouldEqual text


  "RSA" should "fail on large data (due to RSA block size limitation)" in:
    // RSA with 2048 bit key can only encrypt data up to ~245 bytes
    // (2048/8 - padding overhead)
    val largeData = "A" * 246
    val ex = intercept[IllegalBlockSizeException]:
      largeData.encrypted
    ex.getMessage should include("Data must not be longer")

  "RSA" should "hide plaintext" in:
    new String(enc.bytes.mutable)
      .contains(text.getBytes()) shouldBe false
