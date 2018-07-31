package cryptic
package crypto

import org.scalatest._

class AESSpec extends FlatSpec with Matchers {
  import AES._
  implicit private val params: AESParams = AESParams()
  implicit private val password: AESPassphrase = AESPassphrase("secret")
  private val plainText = PlainText("nisse")
  val encryptFun: Encrypt = encrypt // Uses implicit key

  "AES" should "support encryption and decryption" in {
    val encrypted = encryptFun(plainText)

    val decryptFun: Decrypt = decrypt // Uses implicit key
    decryptFun(encrypted) match {
      case Right(actual) ⇒ actual shouldEqual plainText
      case x ⇒ fail(s"does not decrypt: $x")
    }
  }

  "AES" should "hide plaintext" in {
    encryptFun(plainText) match {
      case ct: CipherText ⇒ new String(ct.bytes).contains("nisse".getBytes())
      case _ ⇒ None
    }
  }

}
