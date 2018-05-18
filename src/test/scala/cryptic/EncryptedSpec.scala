package cryptic

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

import cryptic.serialization.{Name, PersonName}
import cryptic.syntax._
import org.scalatest._

class EncryptedSpec extends FlatSpec with Matchers {
  "Fst serializer" should "work when encrypting and decrypting" in {
    import crypto.RSA._
    import serialization.FstSerializer._
    val keySize = 2048
    val keyPairGenerator: KeyPairGenerator = KeyPairGenerator.getInstance("RSA")
    keyPairGenerator.initialize(keySize)
    val keyPair: KeyPair = keyPairGenerator.genKeyPair
    implicit val publicKey: PublicKey = keyPair.getPublic
    val e = PersonName(first = Name("Karl"), last = Name("Nilsson")).encrypted
    val e2 = e.map(_.copy(last = Name("Andersson")))
    val e3 = e.filter(_.last != Name("Nilsson"))
    val e4 = e2.filter(_.last != Name("Nilsson"))
    implicit val privateKey: PrivateKey = keyPair.getPrivate
    e.decrypted shouldEqual Right(PersonName(first = Name("Karl"), last = Name("Nilsson")))
    e2.decrypted shouldEqual Right(PersonName(first = Name("Karl"), last = Name("Andersson")))
    e3.decrypted shouldEqual Left("No such element")
    e4.decrypted shouldEqual Right(PersonName(first = Name("Karl"), last = Name("Andersson")))
  }

  "Case class with encrypted members" should "encrypt and decrypt" in {
    // Serializer that handles case classes
    import serialization.FstSerializer._
    //    import crypto.Caesar._
    //    implicit val key: Key = keygen(1)
    import crypto.Reverse._

    case class Foo(clear: String, secret: Encrypted[String])
    val foo = Foo("clear", "secret".encrypted)
    foo.secret.run.map(_.bytes) match {
      case Right(bytes) ⇒ bytes.toVector shouldEqual Vector(116, 101, 114, 99, 101, 115, 6, -4)
      case _ ⇒ fail("Could not get encrypted bytes")
    }
    // Only need decryption function when decrypting
    foo.secret.decrypted shouldEqual Right("secret")
  }
}
