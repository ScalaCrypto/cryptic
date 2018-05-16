package cryptic

import cryptic.PlainText._
import org.scalatest._

class CaesarSpec extends FlatSpec with Matchers {

  val key1 = Caesar.Key(1)

  "Caesar Encryptor" should "encrypt to shifted string" in {
    Caesar.encryptor(key1).encrypt(PlainText("kalle")) shouldEqual CipherText("lbmmf".getBytes())
  }
  "Caesar Decryptor" should "decrypt to shifted string" in {
    Caesar.decryptor(key1).decrypt(CipherText("lbmmf".getBytes())) map (_.toList) shouldEqual Right(PlainText("kalle").toList)
  }

  "Caesar Encrypted" should "support encryption and decryption" in {
    implicit val decryptor: Decryptor = Caesar.decryptor(key1)
    encrypt(key1, "nisse").decrypted match {
      case Right(decrypted) ⇒ decrypted shouldEqual "nisse"
      case x ⇒ fail(s"does not decrypt: $x")
    }
  }

  "Caesar Encrypted" should "hide plaintext" in {
    encrypt(key1, "nisse") match {
      case Encrypted.Value(ct) ⇒ new String(ct).contains("nisse")
      case _ ⇒ None
    }
  }

  "Caesar zero" should "not be valid" in {
   assertThrows[IllegalArgumentException] { Caesar.Key(0) }
  }

  private def encrypt(key: Caesar.Key, plain: String) = {
    implicit val ce: Encryptor = Caesar.encryptor(key)
    Encrypted[String](plain)
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
