package cryptic
package crypto
package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class CaesarAADSpec extends AnyFlatSpec with Matchers with TryValues:
  import CaesarAAD.{*, given}
  import cryptic.codec.default.given
  import cryptic.Functor.tryFunctor
  given keys: Keys = CaesarAAD.Keys(100 -> 1, 400 -> 2)

  private val text = "secret"
  private val encrypted = text.encrypted(100.toAAD)
  "CaesarAAD Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted match
      case Success(decrypted) => decrypted shouldEqual text
      case x                  => fail(s"does not decrypt: $x")

  "CaesarAAD Encrypted" should "hide plaintext" in:
    encrypted.bytes.map(b =>
      new String(b.mutable).contains(text)
    ) shouldBe Success(false)

  "CaesarAAD key zero" should "not be valid" in:
    assertThrows[IllegalArgumentException]:
      CaesarAAD.Keys(1 -> 0)

  "CaesarAAD encrypted" should "be rotated" in:
    encrypted.bytes.success.value.toSeq shouldEqual IArray.join(
      100.toAAD,
      "tfdsfu".getBytes.immutable
    )
