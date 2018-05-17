package cryptic
package serialization

import cryptic.crypto.Caesar
import org.scalatest._


case class Name(literal: String)
case class PersonName(first: Name, middle: Option[Name] = None, last: Name)
case class EmailAddress(literal: String)

case class User(id: Long, alias: String, name: PersonName, email: EmailAddress)

class FstSerializerSpec extends FlatSpec with Matchers {
  import FstSerializer._

  "Fst serializer" should "serialize string and then deserialize back to original string" in {
    val pt = serializer.serialize("kalle")
    pt shouldNot equal(PlainText("kalle"))
    val o = serializer.deserialize(pt)
    o shouldEqual Right("kalle")
  }

  "Fst serializer" should "serialize user and deserialize back to original user" in {
    val user = User(1, "kalle", PersonName(first = Name("Karl") ,last = Name("Nilsson")), EmailAddress("kalle@nilsson.se"))
    val pt = serializer.serialize(user)
    val o = serializer.deserialize(pt)
    o shouldEqual Right(user)
  }
}
