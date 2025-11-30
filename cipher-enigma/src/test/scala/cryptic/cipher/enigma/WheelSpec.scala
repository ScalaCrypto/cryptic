package cryptic.cipher.enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WheelSpec extends AnyFlatSpec with Matchers:
  behavior of "Rotor.apply"

  it should "construct a rotor from valid notch and wiring strings" in:
    val r = Wheel("Custom", "EKMFLGDQVZNTOWYHXUSPAIBRCJ", "R")
    r.carry('R'.glyph) shouldBe true

  it should "reject an empty (or entirely invalid) notch" in:
    intercept[IllegalArgumentException] {
      Wheel("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "")
    }
    intercept[IllegalArgumentException] {
      Wheel("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "--- __")
    }

  it should "reject a notch with repeating characters" in:
    intercept[IllegalArgumentException] {
      Wheel("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "AA")
    }

  it should "require wiring length to equal Glyph.mod (26)" in:
    // Too short (25)
    intercept[IllegalArgumentException] {
      Wheel("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXY", "A")
    }
    // Too long (27)
    intercept[IllegalArgumentException] {
      Wheel("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXYZA", "A")
    }

  behavior of "Rotor.apply(name:String)"

  it should "return predefined rotors by name I..VI" in:
    Wheel("I").toString shouldBe "I"
    Wheel("II").toString shouldBe "II"
    Wheel("III").toString shouldBe "III"
    Wheel("IV").toString shouldBe "IV"
    Wheel("V").toString shouldBe "V"
    Wheel("VI").toString shouldBe "VI"

  it should "throw for unknown rotor names" in:
    intercept[IllegalArgumentException] {
      Wheel("VII")
    }
    intercept[IllegalArgumentException] {
      Wheel("")
    }
