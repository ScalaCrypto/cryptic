package cryptic
package crypto
package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

<<<<<<<< HEAD:core/src/test/scala/cryptic/crypto/demo/ManifestCaesarSpec.scala
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
========
class AADCaesarSpec extends AnyFlatSpec with Matchers:
  import AADCaesar.{*, given}
  import cryptic.codec.default.given

  given keys: Keys = AADCaesar.Keys(100 -> 1, 400 -> 2)

  private val text = "secret"
  private val encrypted = text.encrypted(100.toAAD)
  "AADCaesar Encrypted" should "support encryption and decryption" in:
    encrypted.decrypted match
      case Success(decrypted) => decrypted shouldEqual text
      case x                  => fail(s"does not decrypt: $x")

  "AADCaesar Encrypted" should "hide plaintext" in:
    new String(encrypted.bytes.mutable)
      .contains(text) shouldBe false
>>>>>>>> c8dc0a5 (removed relase, it's replaced by ci):core/src/test/scala/cryptic/crypto/demo/AADCaesarSpec.scala

  "AADCaesar key zero" should "not be valid" in:
    assertThrows[IllegalArgumentException]:
      AADCaesar.Keys(1 -> 0)

<<<<<<<< HEAD:core/src/test/scala/cryptic/crypto/demo/ManifestCaesarSpec.scala
  "ManifestCaesar encrypted" should "be rotated" in:
    encrypted.bytes.success.value.toSeq shouldEqual IArray.join(
      100.toManifest,
========
  "AADCaesar encrypted" should "be rotated" in:
    encrypted.bytes.mutable shouldEqual IArray.join(
      100.toAAD,
>>>>>>>> c8dc0a5 (removed relase, it's replaced by ci):core/src/test/scala/cryptic/crypto/demo/AADCaesarSpec.scala
      "tfdsfu".getBytes.immutable
    )
