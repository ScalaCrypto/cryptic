package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class MultiCaesarSpec extends AnyFlatSpec with Matchers:
  import MultiCaesar.{*, given}
  import cryptic.codec.default.given
  given keys: Keys = MultiCaesar.Keys(1 -> 2, 12 -> 1)

  private val text = "nisse"
  private val encrypted = text.encrypted(12.toManifest)
  "MultiCaesar Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted match
      case Success(decrypted) ⇒ decrypted shouldEqual text
      case x ⇒ fail(s"does not decrypt: $x")

  "MultiCaesar Encrypted" should "hide plaintext" in:
    new String(encrypted.bytes.mutable)
      .contains(text) shouldBe false

  "MultiCaesar key zero" should "not be valid" in:
    assertThrows[IllegalArgumentException]:
      MultiCaesar.Keys(1 -> 0)

  "MultiCaesar encrypted" should "be rotated" in:
    encrypted.bytes.mutable shouldEqual IArray.join(
      12.toManifest,
      "ojttf".getBytes.immutable
    )
