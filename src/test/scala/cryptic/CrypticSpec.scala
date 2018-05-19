package cryptic

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

import cryptic.crypto.RSA
import cryptic.serialization.{Name, PersonName}
import cryptic.syntax._
import org.scalatest._

class CrypticSpec extends FlatSpec with Matchers with EitherValues {
  "Fst serializer" should "work when encrypting and decrypting" in {
    import crypto.RSA._
    import serialization.Fst._
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
    import serialization.Fst._
    //    import crypto.Caesar._
    //    implicit val key: Key = keygen(1)
    import crypto.Reverse._

    case class Foo(clear: String, secret: Encrypted[String])
    val foo = Foo("clear", "secret".encrypted)
    foo.secret.bytes shouldEqual Array(116, 101, 114, 99, 101, 115, 6, -4)
    // Only need decryption function when decrypting
    foo.secret.decrypted shouldEqual Right("secret")
  }

  "Encrypted bytes" should "be callable without encrypt/decrypt in scope" in {
    e: Encrypted[String] =>
      e.bytes
  }
}
