package cryptic
package crypto

import java.security.{KeyPair, PrivateKey, PublicKey}

import cryptic.serialization.StringSerializer
import org.scalatest._

class RSASpec extends FlatSpec with Matchers {

  import java.security.KeyPairGenerator

  import RSA._
  import StringSerializer._

  val keySize = 2048
  val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
  keyPairGenerator.initialize(keySize)
  private val keyPair: KeyPair = keyPairGenerator.genKeyPair
  implicit val publicKey: PublicKey = keyPair.getPublic

  "RSA Encrypted" should "support encryption and decryption" in {
    val encrypted = Encrypted[String]("nisse")
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    encrypted.decrypted match {
      case Right(decrypted) ⇒ decrypted shouldEqual "nisse"
      case x ⇒ fail(s"does not decrypt: $x")
    }
  }

  "RSA Encrypted" should "hide plaintext" in {
    // Note no need for the private key when encrypting
    Encrypted[String]("nisse") match {
      case Encrypted.Value(ct) ⇒ new String(ct.bytes).contains("nisse")
      case _ ⇒ None
    }
  }

  "RSA" should "same value should be equal in encrypted space without decryption key" in {
    val enc1 = Encrypted[String]("nisse")
    val enc2 = Encrypted[String]("nisse")
    enc1 shouldEqual enc2
  }

  "RSA" should "different values should no be equal" in {
    val enc1 = Encrypted[String]("nisse")
    val enc2 = Encrypted[String]("kalle")
    (enc1 == enc2) shouldBe false
  }

}
