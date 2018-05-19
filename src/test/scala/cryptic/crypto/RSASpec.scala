package cryptic
package crypto

import java.security.{KeyPair, PrivateKey, PublicKey}

import cryptic.serialization.FstSerializer
import org.scalatest._

class RSASpec extends FlatSpec with Matchers {

  import RSA._
  import FstSerializer._

  private val keyPair: KeyPair = keygen(2048)
  implicit val publicKey: PublicKey = keyPair.getPublic

  "RSA Encrypted" should "support encryption and decryption" in {
    val encrypted = Encrypted("nisse")
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match {
      case Right(decrypted) ⇒ decrypted shouldEqual "nisse"
      case x ⇒ fail(s"does not decrypt: $x")
    }
  }

  "RSA Encrypted" should "hide plaintext" in {
    // Note no need for the private key when encrypting
    Encrypted("nisse") match {
      case Encrypted(ct) ⇒ new String(ct.bytes).contains("nisse")
      case _ ⇒ None
    }
  }

  "RSA" should "same value should be equal in encrypted space without decryption key" in {
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("nisse")
    enc1 shouldEqual enc2
  }

  "RSA" should "different values should no be equal" in {
    val enc1 = Encrypted("nisse")
    val enc2 = Encrypted("kalle")
    (enc1 == enc2) shouldBe false
  }

}
