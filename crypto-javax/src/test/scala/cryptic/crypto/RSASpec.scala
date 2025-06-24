package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.util.Success

class RSASpec extends AnyFlatSpec with Matchers:
  import RSA.*
  import RSA.given
  private val keyPair: KeyPair = keygen(2048)
  implicit val publicKey: PublicKey = keyPair.getPublic
  private val text = "nisse"
  private val plainText = PlainText(text)
  val encryptFun: Encrypt = encrypt // Uses implicit key

  "RSA" should "support encryption and decryption" in:
    // Note no need for the private key when encrypting
    val encrypted = encryptFun(plainText)

    implicit val privateKey: PrivateKey = keyPair.getPrivate
    val decryptFun: Decrypt = decrypt // Uses implicit key
    decryptFun(encrypted) match
      case Success(actual) ⇒ actual shouldEqual plainText
      case x ⇒ fail(s"does not decrypt: $x")

  "RSA" should "hide plaintext" in:
    new String(encryptFun(plainText).bytes)
      .contains("nisse".getBytes()) shouldBe false
