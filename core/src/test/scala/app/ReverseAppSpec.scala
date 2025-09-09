package app

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.concurrent.Future
import scala.util.{Success, Try}

class ReverseAppSpec extends AsyncFlatSpec with Matchers:
  import cryptic.{*, given}
  import cryptic.codec.default.given
  import cryptic.crypto.Reverse.given

  val clear = "secret"
  val encrypted: Future[Encrypted[String]] = clear.encrypted
  val decrypted: Future[String] = encrypted.flatMap(_.decrypted)

  "Cryptic" should "encrypt" in:
    encrypted.map(_.bytes should not equal clear.getBytes)

  "Cryptic" should "decrypt" in:
    decrypted.map(_ shouldEqual clear)

  case class Person(id: Long, email: Encrypted[String])

  "Person" should "handle encryption" in:
    val id = 17
    val email = "martin@scalacrypto.org"
    val person = email.encrypted(Person(id, _))
    person.map(_.email.bytes should not equal email.getBytes())
    person.flatMap(_.email.contains(email).map(_ shouldBe true))
    person.map(_.toString shouldBe "Person(17,Encrypted(CipherText(0x67726f2e6f7470797263616c616373406e697472616d)))")

  "PlainText" should "be easy" in:
    PlainText(clear).bytes shouldBe clear.getBytes()
    val enc = clear.encrypted
    enc.map(_.bytes shouldBe clear.getBytes.reverse)
