package cryptic

import cryptic.PlainText._
import org.scalatest._

class CaesarSpec extends FlatSpec with Matchers {

  "Caesar Encryptor" should "encrypt to shifted string" in {
    Caesar.encryptor(Caesar.Key(0)).encrypt(PlainText("kalle")) shouldEqual CipherText("kalle".getBytes())
    Caesar.encryptor(Caesar.Key(1)).encrypt(PlainText("kalle")) shouldEqual CipherText("lbmmf".getBytes())
  }
  "Caesar Decryptor" should "decrypt to shifted string" in {
    Caesar.decryptor(Caesar.Key(0)).decrypt(CipherText("kalle".getBytes())) map(_.toList)  shouldEqual Right(PlainText("kalle").toList)
    Caesar.decryptor(Caesar.Key(1)).decrypt(CipherText("lbmmf".getBytes())) map(_.toList) shouldEqual Right(PlainText("kalle").toList)
  }

  "Caesar Encrypted" should "support encryption and decryption" in {
    val key = Caesar.Key(0)
    implicit val ce: Encryptor = Caesar.encryptor(key)
    val e = Encrypted[String]("nisse") match { case Encrypted.Value(ct) ⇒ }
    e.asInstanceOf[Encrypted.Value[String]].cipherText .contains("nisse") shouldBe false

    implicit val cd: Decryptor = Caesar.decryptor(key)
    val decrypted: Either[String, String] = e.decrypted
    decrypted.contains("nisse") shouldBe true
  }
  /*
  def createUser: User = {
    implicit val key: Caesar.Key = Cryptos.Caesar.Key(1)
    User(1, "kalle", Encrypted(PersonName(first = Name("Karl") ,last = Name("Nilsson"))), Encrypted(EmailAddress("kalle@nilsson.se")))
  }
  "Caesar crypto" should "work when used with correct key" in {
    implicit val key: Caesar.Key = Cryptos.Caesar.Key(1)
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
