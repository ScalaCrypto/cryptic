package cryptic
package crypto

import cryptic.serialization.StringSerializer
import org.scalatest._

class CaesarSpec extends FlatSpec with Matchers {
  import Caesar._
  import StringSerializer._
  implicit val key1: Key = Caesar.Key(1)

  "Caesar Encrypted" should "support encryption and decryption" in {
    Encrypted("nisse").decrypted match {
      case Right(decrypted) ⇒ decrypted shouldEqual "nisse"
      case x ⇒ fail(s"does not decrypt: $x")
    }
  }

  "Caesar Encrypted" should "hide plaintext" in {
    Encrypted("nisse") match {
      case Encrypted(ct) ⇒ new String(ct.bytes).contains("nisse")
      case _ ⇒ None
    }
  }

  "Caesar zero" should "not be valid" in {
   assertThrows[IllegalArgumentException] { Caesar.Key(0) }
  }

}
