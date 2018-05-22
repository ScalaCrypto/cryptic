package cryptic
package crypto

import java.security.{KeyPair, PrivateKey, PublicKey}

import org.scalatest._

class RSASpec extends FlatSpec with Matchers {

  import RSA._

  private val keyPair: KeyPair = keygen(2048)
  implicit val publicKey: PublicKey = keyPair.getPublic
  private val plainText = PlainText("nisse")
  val encryptFun: Encrypt = encrypt // Uses implicit key

  "RSA" should "support encryption and decryption" in {
    val encrypted = encryptFun(plainText)

    implicit val privateKey: PrivateKey = keyPair.getPrivate
    val decryptFun: Decrypt = decrypt // Uses implicit key
    decryptFun(encrypted) match {
      case Right(actual) ⇒ actual shouldEqual plainText
      case x ⇒ fail(s"does not decrypt: $x")
    }
  }

  "RSA" should "hide plaintext" in {
    // Note no need for the private key when encrypting
    encryptFun(plainText) match {
      case ct:CipherText ⇒ new String(ct.bytes).contains("nisse".getBytes())
      case _ ⇒ None
    }
  }

}
