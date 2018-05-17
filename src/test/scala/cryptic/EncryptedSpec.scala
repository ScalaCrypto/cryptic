package cryptic

import org.scalatest._
import PlainText._

class EncryptedSpec extends FlatSpec with Matchers {
  case class Name(literal: String)
  case class PersonName(first: Name, middle: Option[Name] = None, last: Name)
  case class EmailAddress(literal: String)

  case class User(
    id: Long,
    alias: String,
    name: Encrypted[PersonName],
    email: Encrypted[EmailAddress])

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
