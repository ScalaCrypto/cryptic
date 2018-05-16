package cryptic

import org.scalatest._
import Encrypted.Coders._
import Encrypted.Cryptos._
import cryptic.Encrypted.Cryptos
import cryptic.Encrypted.Cryptos.Ceasar

class EncryptedSpec extends FlatSpec with Matchers {
  case class Name(literal: String)
  case class PersonName(first: Name, middle: Option[Name] = None, last: Name)
  case class EmailAddress(literal: String)

  case class User(
    id: Long,
    alias: String,
    name: Encrypted[PersonName],
    email: Encrypted[EmailAddress])

  "String Encoder" should "encode to equal" in {
    stringEncoder.encode("kalle") shouldEqual PlainText("kalle")
  }
  "String Decoder" should "decode to equal" in {
    stringDecoder.decode(PlainText("kalle")) shouldEqual Right("kalle")
  }

  "Ceasar Encryptor" should "encrypt to shifted string" in {
    Cryptos.Ceasar.encryptor(Cryptos.Ceasar.Key(0)).encrypt(PlainText("kalle")) shouldEqual CipherText("kalle")
    Cryptos.Ceasar.encryptor(Cryptos.Ceasar.Key(1)).encrypt(PlainText("kalle")) shouldEqual CipherText("lbmmf")
  }
  "Ceasar Decryptor" should "decrypt to shifted string" in {
    Cryptos.Ceasar.decryptor(Cryptos.Ceasar.Key(0)).decrypt(CipherText("kalle")) shouldEqual Right(PlainText("kalle"))
    Cryptos.Ceasar.decryptor(Cryptos.Ceasar.Key(1)).decrypt(CipherText("lbmmf")) shouldEqual Right(PlainText("kalle"))
  }
  /*
  def createUser: User = {
    implicit val key: Ceasar.Key = Cryptos.Ceasar.Key(1)
    User(1, "kalle", Encrypted(PersonName(first = Name("Karl") ,last = Name("Nilsson"))), Encrypted(EmailAddress("kalle@nilsson.se")))
  }
  "Ceasar crypto" should "work when used with correct key" in {
    implicit val key: Ceasar.Key = Cryptos.Ceasar.Key(1)
    val user = createUser
    user.email.map(_.literal).decrypted shouldEqual "kalle@nilsson.se"
  }
  "Ceasar crypto" should "not work when used with incorrect key" in {
    implicit val key: Ceasar.Key = Cryptos.Ceasar.Key(2)
    val user = createUser
    user.email.map(_.literal).decrypted shouldEqual "kalle@nilsson.se"
  }
  */
}
