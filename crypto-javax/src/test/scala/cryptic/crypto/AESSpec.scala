package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class AESSpec extends AnyFlatSpec with Matchers {
  import AES._
  implicit private val params: AESParams = AESParams()
  implicit private val password: AESPassphrase = AESPassphrase("secret")
  private val plainText = PlainText("nisse")
  val encryptFun: Encrypt = encrypt // Uses implicit key

  "AES" should "support encryption and decryption" in {
    val encrypted = encryptFun(plainText)

    val decryptFun: Decrypt = decrypt // Uses implicit key
    decryptFun(encrypted) match {
      case Success(actual) ⇒ actual shouldEqual plainText
      case x ⇒ fail(s"does not decrypt: $x")
    }
  }

  "AES" should "hide plaintext" in {
    encryptFun(plainText) match {
      case ct: CipherText ⇒ new String(ct.bytes).contains("nisse".getBytes())
      case _ ⇒ None
    }
  }

  "AESParams keyspecLength" should
    "only allow 126, 192, 256" in {
      AESParams(keyspecLength = 192) should not be null
    }

  "AESParams keyspecLength" should
    "not allow 127" in {
      val exception = intercept[AssertionError] {
        AESParams(keyspecLength = 127)
      }
      exception.getMessage shouldBe "assertion failed: Invalid keyspecLength: 127. Allowed values are 128, 192, 256."
    }

}
