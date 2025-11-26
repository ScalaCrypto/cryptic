package cryptic.cipher.enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RotorSpec extends AnyFlatSpec with Matchers:
  behavior of "Rotor.apply"

  it should "construct a rotor from valid notch and wiring strings" in:
    val r = Rotor("Custom", "EKMFLGDQVZNTOWYHXUSPAIBRCJ", "R")
    r.carry('R'.glyph) shouldBe true

  it should "reject an empty (or entirely invalid) notch" in:
    intercept[IllegalArgumentException] {
      Rotor("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "")
    }
    intercept[IllegalArgumentException] {
      Rotor("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "--- __")
    }

  it should "reject a notch with repeating characters" in:
    intercept[IllegalArgumentException] {
      Rotor("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "AA")
    }

  it should "require wiring length to equal Glyph.mod (26)" in:
    // Too short (25)
    intercept[IllegalArgumentException] {
      Rotor("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXY", "A")
    }
    // Too long (27)
    intercept[IllegalArgumentException] {
      Rotor("Custom", "ABCDEFGHIJKLMNOPQRSTUVWXYZA", "A")
    }

  behavior of "Rotor.apply(name:String)"

  it should "return predefined rotors by name I..VI" in:
    Rotor("I").toString shouldBe "I"
    Rotor("II").toString shouldBe "II"
    Rotor("III").toString shouldBe "III"
    Rotor("IV").toString shouldBe "IV"
    Rotor("V").toString shouldBe "V"
    Rotor("VI").toString shouldBe "VI"

  it should "throw for unknown rotor names" in:
    intercept[IllegalArgumentException] {
      Rotor("VII")
    }
    intercept[IllegalArgumentException] {
      Rotor("")
    }
