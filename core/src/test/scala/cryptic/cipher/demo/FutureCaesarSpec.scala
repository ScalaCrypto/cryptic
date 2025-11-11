package cryptic
package cipher
package demo

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future
import scala.util.{Success, Try}

class FutureCaesarSpec extends AsyncFlatSpec with Matchers:
  import cryptic.codec.default.given
  import FutureCaesar.{*, given}
  import cryptic.Functor.futureFunctor

  given key1: Key = FutureCaesar.Key(1)

  private val text = "nisse"
  private val encrypted: Encrypted[Future, String] = text.encrypted
  "FutureCaesar Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted.map(_ shouldEqual text)

  "FutureCaesar Encrypted" should "hide plaintext" in:
    encrypted.bytes.map(b =>
      new String(b.mutable).contains(text) shouldBe false
    )

  "FutureCaesar key zero" should "not be valid" in:
    given key0: Key = Key(0)
    recoverToSucceededIf[IllegalArgumentException]:
      text.encrypted.bytes

  "FutureCaesar Encrypted" should "be rotated" in:
    encrypted.bytes.map(_.toSeq shouldEqual "ojttf".getBytes)
