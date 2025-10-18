package cryptic
package crypto
package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class ManifestCaesarSpec extends AnyFlatSpec with Matchers with TryValues:
  import ManifestCaesar.{*, given}
  import cryptic.codec.default.given
  import cryptic.Functor.tryFunctor
  given keys: Keys = ManifestCaesar.Keys(100 -> 1, 400 -> 2)

  private val text = "secret"
  private val encrypted: Encrypted[Try, String] = text.encrypted(100.toManifest)
  "ManifestCaesar Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted.success.value shouldEqual text

  "ManifestCaesar Encrypted" should "hide plaintext" in:
    encrypted.bytes.map(b =>
      new String(b.mutable).contains(text)
    ) shouldBe Success(false)

  "ManifestCaesar key zero" should "not be valid" in:
    assertThrows[IllegalArgumentException]:
      ManifestCaesar.Keys(1 -> 0)

  "ManifestCaesar encrypted" should "be rotated" in:
    encrypted.bytes.success.value.toSeq shouldEqual IArray.join(
      100.toManifest,
      "tfdsfu".getBytes.immutable
    )
