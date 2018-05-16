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
    name: Encrypted[PersonName, _],
    email: Encrypted[EmailAddress, _])

  "String encrypter" should "encrypt to equal" in {
    stringEncrypter.encrypt("kalle")("K") shouldEqual Secret("kalle")
  }
  "String decrypter" should "decrypt to equal" in {
    stringDecrypter.decrypt(Secret("kalle"))("K") shouldEqual Some("kalle")
  }

  "Ceasar string encrypter" should "encrypt to shifted string" in {
    Crypto.Ceasar.stringEncrypter.encrypt("kalle")(Crypto.Ceasar.Key(0)) shouldEqual Secret("kalle")
    Crypto.Ceasar.stringEncrypter.encrypt("kalle")(Crypto.Ceasar.Key(1)) shouldEqual Secret("lbmmf")
  }
  "Ceasar string decrypter" should "decrypt to shifted string" in {
    Crypto.Ceasar.stringDecrypter.decrypt(Secret("kalle"))(Crypto.Ceasar.Key(0)) shouldEqual Some("kalle")
    Crypto.Ceasar.stringDecrypter.decrypt(Secret("lbmmf"))(Crypto.Ceasar.Key(1)) shouldEqual Some("kalle")
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
