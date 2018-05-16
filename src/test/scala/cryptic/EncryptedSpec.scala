package cryptic

import org.scalatest._
import Encrypted.Implicits._
import cryptic.Encrypted.Crypto
import cryptic.Encrypted.Crypto.Ceasar

class EncryptedSpec extends FlatSpec with Matchers {
  case class Name(literal: String)
  case class PersonName(first: Name, middle: Option[Name] = None, last: Name)
  case class EmailAddress(literal: String)

  case class User(
    id: Long,
    alias: String,
    name: Encrypted[PersonName],
    email: Encrypted[EmailAddress])

  "String Encryptor" should "encrypt to equal" in {
    stringEncryptor.encrypt("kalle") shouldEqual CipherText("kalle")
  }
  "String Decryptor" should "decrypt to equal" in {
    stringDecryptor.decrypt(CipherText("kalle")) shouldEqual Some("kalle")
  }

  "Ceasar string Encryptor" should "encrypt to shifted string" in {
    Crypto.Ceasar.stringEncryptor(Crypto.Ceasar.Key(0)).encrypt("kalle") shouldEqual CipherText("kalle")
    Crypto.Ceasar.stringEncryptor(Crypto.Ceasar.Key(1)).encrypt("kalle") shouldEqual CipherText("lbmmf")
  }
  "Ceasar string Decryptor" should "decrypt to shifted string" in {
    Crypto.Ceasar.stringDecryptor(Crypto.Ceasar.Key(0)).decrypt(CipherText("kalle")) shouldEqual Some("kalle")
    Crypto.Ceasar.stringDecryptor(Crypto.Ceasar.Key(1)).decrypt(CipherText("lbmmf")) shouldEqual Some("kalle")
  }
  def createUser: User = {
    implicit val key: Ceasar.Key = Crypto.Ceasar.Key(1)
    User(1, "kalle", Encrypted(PersonName(first = Name("Karl") ,last = Name("Nilsson"))), Encrypted(EmailAddress("kalle@nilsson.se")))
  }
  "Ceasar crypto" should "work when used with correct key" in {
    implicit val key: Ceasar.Key = Crypto.Ceasar.Key(1)
    val user = createUser
    user.email.map(_.literal).decrypted shouldEqual "kalle@nilsson.se"
  }
  "Ceasar crypto" should "not work when used with incorrect key" in {
    implicit val key: Ceasar.Key = Crypto.Ceasar.Key(2)
    val user = createUser
    user.email.map(_.literal).decrypted shouldEqual "kalle@nilsson.se"
  }


}
