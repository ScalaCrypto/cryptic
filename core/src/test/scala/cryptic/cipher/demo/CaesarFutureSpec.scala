package cryptic
package cipher
package demo

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future
import scala.util.{Success, Try}

class CaesarFutureSpec extends AsyncFlatSpec with Matchers:
  import cryptic.codec.default.given
  import CaesarFuture.{*, given}

  given Key(1)

  private val text = "nisse"
  private val encrypted: Encrypted[Future, String] = text.encrypted

  it should "support encryption and decryption" in:
    encrypted.decrypted.map(_ shouldEqual text)

  it should "hide plaintext" in:
    encrypted.bytes.map(b =>
      new String(b.mutable).contains(text) shouldBe false
    )

  it should "reject key 0" in:
    given key0: Key = Key(0)
    recoverToSucceededIf[IllegalArgumentException]:
      text.encrypted.bytes

  it should "rotate a encryption" in:
    encrypted.bytes.map(_.toSeq shouldEqual "ojttf".getBytes)
