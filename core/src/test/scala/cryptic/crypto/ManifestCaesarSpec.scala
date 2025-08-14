package cryptic
package crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class ManifestCaesarSpec extends AnyFlatSpec with Matchers:
  import ManifestCaesar.{*, given}
  import cryptic.codec.default.given
  given keys: Keys = ManifestCaesar.Keys(1 -> 2, 12 -> 1)

  private val text = "nisse"
  private val encrypted = text.encrypted(12.toManifest)
  "ManifestCaesar Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted match
      case Success(decrypted) => decrypted shouldEqual text
      case x                  => fail(s"does not decrypt: $x")

  "ManifestCaesar Encrypted" should "hide plaintext" in:
    new String(encrypted.bytes.mutable)
      .contains(text) shouldBe false

  "ManifestCaesar key zero" should "not be valid" in:
    assertThrows[IllegalArgumentException]:
      ManifestCaesar.Keys(1 -> 0)

  "ManifestCaesar encrypted" should "be rotated" in:
    encrypted.bytes.mutable shouldEqual IArray.join(
      12.toManifest,
      "ojttf".getBytes.immutable
    )
