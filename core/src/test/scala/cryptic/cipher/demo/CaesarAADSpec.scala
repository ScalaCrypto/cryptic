package cryptic
package cipher
package demo

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.util.{Success, Try}

class CaesarAADSpec extends AnyFlatSpec with Matchers with TryValues:
  import CaesarAAD.{*, given}
  import cryptic.codec.default.given
  given keys: Keys = CaesarAAD.Keys("Pompey" -> 1, "Crassus" -> 2)

  private val text = "secret"
  private val pompey = text.encrypted("Pompey".aad)
  private val crassus = text.encrypted("Crassus".aad)

  it should "support encryption and decryption to Pompey" in:
    pompey.decrypted.success.value shouldEqual text

  it should "support encryption and decryption to Crassus" in:
    crassus.decrypted.success.value shouldEqual text

  it should "hide plaintext" in:
    pompey.bytes.map(b =>
      new String(b.mutable).contains(text)
    ) shouldBe Success(false)

  it should "key zero not be valid" in:
    assertThrows[IllegalArgumentException]:
      CaesarAAD.Keys("Self" -> 0)

  it should "Fail on nonexisting key" in:
    text.encrypted("Brutus".aad).bytes.failed

  it should "rotate the plaintext in the ciphertext" in:
    pompey.bytes.success.value.toSeq shouldEqual IArray.join(
      "Pompey".bytes,
      "tfdsfu".getBytes.immutable
    )
