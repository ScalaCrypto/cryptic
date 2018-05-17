package cryptic
package serialization

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}
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

  "Fst serializer" should "work when encrypting and decrypting" in {
    import cryptic.crypto.RSA._
    val keySize = 2048
    val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
    keyPairGenerator.initialize(keySize)
    val keyPair: KeyPair = keyPairGenerator.genKeyPair
    implicit val publicKey: PublicKey = keyPair.getPublic
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    val e = Encrypted(PersonName(first = Name("Karl"), last = Name("Nilsson")))
    val e2 = e.map(_.copy(last = Name("Andersson")))
    val e3 = e.filter(_.last != Name("Nilsson"))
    val e4 = e2.filter(_.last != Name("Nilsson"))
    e.decrypted shouldEqual Right(PersonName(first = Name("Karl"), last = Name("Nilsson")))
    e2.decrypted shouldEqual Right(PersonName(first = Name("Karl"), last = Name("Andersson")))
    e3.decrypted shouldEqual Right(None)
    e4.decrypted shouldEqual Right(Some(PersonName(first = Name("Karl"), last = Name("Andersson"))))
  }
}
