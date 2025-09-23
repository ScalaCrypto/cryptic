package cryptic
package crypto

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.*

class FutureCaesarSpec extends AsyncFlatSpec with Matchers with ScalaFutures:
  import cryptic.codec.default.given
  import FutureCaesar.*
  import FutureCaesar.given

  given ExecutionContext = ExecutionContext.global
  given key1: Key = FutureCaesar.Key(1)

  private val text = "nisse"
  private val enc: Future[Encrypted[String]] = text.encrypted

  "FutureCaesar" should "support encryption and decryption" in:
    enc
      .flatMap(_.decrypted)
      .map: dec =>
        dec shouldEqual text

  "FutureCaesar" should "hide plaintext" in:
    enc.map(encrypted =>
      new String(encrypted.bytes.mutable).contains(text) shouldBe false
    )

  "FutureCaesar key zero" should "not be valid" in:
    assertThrows[IllegalArgumentException]:
      FutureCaesar.Key(0)

  "FutureCaesar" should "be rotated" in:
    enc.map(encrypted => encrypted.bytes.mutable shouldEqual "ojttf".getBytes)
