package app

import cryptic.crypto.Aes.{*, given}
import org.scalatest.TryValues
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.security.{KeyPair, PrivateKey, PublicKey}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

class DefaultAppSpec extends AsyncFlatSpec with Matchers:
  import cryptic.default.{given, *}

  val keyPair: KeyPair = newKeyPair(2048)
  given ec: ExecutionContext = ExecutionContext.global
  given publicKey: PublicKey = keyPair.getPublic
  given privateKey: PrivateKey = keyPair.getPrivate
  val clear = "secret"
  val encrypted: Future[Encrypted[String]] = clear.encrypted
  val decrypted: Future[String] = encrypted.flatMap(_.decrypted)

  "Cryptic" should "encrypt" in:
    encrypted.map(_.bytes should not equal clear.getBytes)

  "Cryptic" should "decrypt" in:
    decrypted.map(_ shouldEqual clear)

  case class Person(id: Long, email: Encrypted[String])

  val id = 17
  val email = "martin@scalacrypto.org"
  val person: Future[Person] = email.encrypted(Person(id, _))

  "Email" should "be encrypted" in:
    person.map(p => p.email.bytes.unsafeArray should not equal email.getBytes())
    person.flatMap(_.email.contains(email).map(_ shouldBe true))
    person.map(_.toString should startWith("Person(17,Encrypted(CipherText(0x"))

  "Email" should "be decrypted" in:
    person.flatMap(_.email.decrypted.map(_ shouldEqual email))
