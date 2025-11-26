package cryptic
package cipher
package enigma

import cryptic.cipher.enigma.Reflector.A
import org.scalatest.flatspec.{AnyFlatSpec, AsyncFlatSpec}
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class EnigmaSpec extends AnyFlatSpec with Matchers with TryValues:
  behavior of "Enigma"

  import Enigma.default.{given, *}
  import Functor.tryFunctor

  "Enigma" should "encrypt MARTIN to WFSEXB" in:
    given settings: Settings = Settings('Z')
    val enc: Encrypted[Try, String] =
      "MARTIN".encrypted
    enc.bytes.success.value shouldBe "WFSEXB".bytes

  it should "encrypt A to T when Z" in:
    given settings:Settings  = Settings('Z')
    "A".encrypted.bytes.success.value shouldBe "N".bytes

  it should "encrypt A to T when A" in:
    given settings:Settings  = Settings('A')
    "A".encrypted.bytes.success.value shouldBe "F".bytes

  it should "encrypt A to T when B" in:
    given settings:Settings  = Settings('B')
    "A".encrypted.bytes.success.value shouldBe "T".bytes

  it should "encrypt A to T when C" in:
    given settings:Settings  = Settings('C')
    "A".encrypted.bytes.success.value shouldBe "Z".bytes
