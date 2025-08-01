package cryptic
package crypto

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.Success

class ECSpec extends AnyFlatSpec with Matchers:
  import EC.*
  import EC.given
  private val keyPair: KeyPair = keygen(256)
  implicit val publicKey: PublicKey = keyPair.getPublic
  private val text = "secret"

  "EC" should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    val encrypted: Encrypted[String] = text.encrypted

    given privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match
      case Success(actual) => actual shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "EC" should "hide the plain text" in:
    new String(text.encrypted.bytes)
      .contains(text.getBytes()) shouldBe false
