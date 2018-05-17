package cryptic

import java.security.{KeyPair, KeyPairGenerator, PrivateKey, PublicKey}

import org.scalatest._
import cryptic.serialization.{Name, PersonName}

class EncryptedSpec extends FlatSpec with Matchers {
  "Fst serializer" should "work when encrypting and decrypting" in {
    import serialization.FstSerializer._
    import crypto.RSA._
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
