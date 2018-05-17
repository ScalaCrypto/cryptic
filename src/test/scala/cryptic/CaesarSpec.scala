package cryptic

import cryptic.PlainText._
import cryptic.crypto.Caesar
import org.scalatest._
//import Predef.{$conforms => _}

class CaesarSpec extends FlatSpec with Matchers {
  import Caesar._
  implicit val key1 = Caesar.Key(1)

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

  /*
  def createUser: User = {
    implicit val key: Caesar.Key = Cryptos.key1
    User(1, "kalle", Encrypted(PersonName(first = Name("Karl") ,last = Name("Nilsson"))), Encrypted(EmailAddress("kalle@nilsson.se")))
  }
  "Caesar crypto" should "work when used with correct key" in {
    implicit val key: Caesar.Key = Cryptos.key1
    val user = createUser
    user.email.map(_.literal).decrypted shouldEqual "kalle@nilsson.se"
  }
  "Caesar crypto" should "not work when used with incorrect key" in {
    implicit val key: Caesar.Key = Cryptos.Caesar.Key(2)
    val user = createUser
    user.email.map(_.literal).decrypted shouldEqual "kalle@nilsson.se"
  }
  */
}
