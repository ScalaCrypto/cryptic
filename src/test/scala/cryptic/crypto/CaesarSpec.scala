package cryptic
package crypto

import cryptic.serialization.StringSerializer
import org.scalatest._

class CaesarSpec extends FlatSpec with Matchers {
  import Caesar._
  import StringSerializer._
  implicit val key1: Key = Caesar.Key(1)

  "Caesar Encryptor" should "encrypt to shifted string" in {
    Encrypted[String]("kalle") match {
      case Encrypted.Value(cipherText) => cipherText shouldEqual CipherText("lbmmf".getBytes())
      case _ => fail("bad encryption")
    }
  }
  "Caesar Decryptor" should "decrypt to shifted string" in {
    Encrypted[String](CipherText("lbmmf".getBytes())).decrypted shouldEqual Right("kalle")
  }

  "Caesar Encrypted" should "support encryption and decryption" in {
    Encrypted[String]("nisse").decrypted match {
      case Right(decrypted) ⇒ decrypted shouldEqual "nisse"
      case x ⇒ fail(s"does not decrypt: $x")
    }
  }

  "Caesar Encrypted" should "hide plaintext" in {
    Encrypted[String]("nisse") match {
      case Encrypted.Value(ct) ⇒ new String(ct).contains("nisse")
      case _ ⇒ None
    }
  }

  "Caesar zero" should "not be valid" in {
   assertThrows[IllegalArgumentException] { Caesar.Key(0) }
  }

}
