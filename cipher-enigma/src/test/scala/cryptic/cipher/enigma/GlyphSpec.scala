package cryptic.cipher.enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GlyphSpec extends AnyFlatSpec with Matchers:
  behavior of "Glyph"

  import Glyph.*

  "Glyph.apply(Int)" should "wrap values mod 26" in:
    Glyph(0).char shouldBe 'A'
    Glyph(25).char shouldBe 'Z'
    Glyph(26).char shouldBe 'A'
    Glyph(27).char shouldBe 'B'
    Glyph(-1).char shouldBe 'Z'

  it should "construct from Char" in:
    'A'.glyph.char shouldBe 'A'
    'Z'.glyph.char shouldBe 'Z'
    'B'.glyph.char shouldBe 'B'

  it should "add and subtract with wrap-around" in:
    ('A'.glyph + Glyph(1)).char shouldBe 'B'
    ('Z'.glyph + Glyph(1)).char shouldBe 'A'
    ('B'.glyph - Glyph(1)).char shouldBe 'A'
    ('A'.glyph - Glyph(1)).char shouldBe 'Z'

  it should "increment with ++ and wrap-around" in:
    'A'.glyph.++.char shouldBe 'B'
    'Z'.glyph.++.char shouldBe 'A'
    // Chaining ++ should advance twice
    'Y'.glyph.++.++.char shouldBe 'A'
    // ++ should be equivalent to adding one
    'C'.glyph.++.char shouldBe ('C'.glyph + one).char

  "String.glyph" should "convert a String to Glyphs and back" in:
    val s = "ABZ"
    val g = s.glyph
    g.string shouldBe s

  it should "upper-case lower-case input before mapping to Glyphs" in:
    "abz".glyph.string shouldBe "ABZ"
    "aBz".glyph.string shouldBe "ABZ"
    "martin".glyph.string shouldBe "MARTIN"

  it should "ignore non alphabetic characters using unsafe and keep only letters" in:
    "A-B Z1".glyph.string shouldBe "ABZ"
    "--__ 123".glyph.string shouldBe ""
    "a-b_z".glyph.string shouldBe "ABZ"

  "IArray[Glyph].string" should "handle empty and multi-letter strings" in:
    IArray.empty[Glyph].string shouldBe ""
    "MARTIN".glyph.string shouldBe "MARTIN"

  "Glyph.unsafe(Char)" should "accept uppercase letters" in:
    Glyph.unsafe('A').get.char shouldBe 'A'
    Glyph.unsafe('Z').get.char shouldBe 'Z'

  it should "convert lowercase letters to uppercase and succeed" in:
    Glyph.unsafe('a').get.char shouldBe 'A'
    Glyph.unsafe('m').get.char shouldBe 'M'
    Glyph.unsafe('z').get.char shouldBe 'Z'

  it should "fail for non alphabetic characters with IllegalArgumentException" in:
    val invalids = List('1', ' ', '-', '[', '`', 'å')
    invalids.foreach { c =>
      val t = Glyph.unsafe(c)
      t.isFailure shouldBe true
      t.failed.get shouldBe a [IllegalArgumentException]
    }

  "Glyph.isValid(Char)" should "return true for letters A–Z and a–z" in:
    Glyph.isValid('A') shouldBe true
    Glyph.isValid('Z') shouldBe true
    Glyph.isValid('a') shouldBe true
    Glyph.isValid('z') shouldBe true

  it should "return false for non alphabetic characters" in:
    val invalids = List('1', ' ', '-', '[', '`', 'å')
    invalids.foreach { c =>
      Glyph.isValid(c) shouldBe false
    }

  "Glyph extension methods" should "convert between Char and String" in :
    val g = 'A'.glyph
    g.char shouldBe 'A'
    g.string shouldBe "A"
    Glyph(25).string shouldBe "Z"
    "ABC".glyph.string shouldBe "ABC"
    Seq('A'.glyph, 'B'.glyph, 'C'.glyph).string shouldBe "ABC"

  "Rotor.previousCarry" should "be true when current pos is one after the notch (single notch)" in:
    // Wheel I has notch at 'R' -> previousCarry true at pos 'S'
    val r1 = Rotor("I", 'A', 'S')
    r1.previousCarry shouldBe true
    // At the notch itself ('R') it's false for previousCarry
    Rotor("I", 'A', 'R').previousCarry shouldBe false
    // One past 'S' ('T') is also false
    Rotor("I", 'A', 'T').previousCarry shouldBe false

  it should "handle multiple notches correctly (VI: A and N)" in:
    // Wheel VI has notches at 'A' and 'N' -> previousCarry true at 'B' and 'O'
    Rotor("VI", 'A', 'B').previousCarry shouldBe true
    Rotor("VI", 'A', 'O').previousCarry shouldBe true
    // At the notch positions themselves, previousCarry should be false
    Rotor("VI", 'A', 'A').previousCarry shouldBe false
    Rotor("VI", 'A', 'N').previousCarry shouldBe false
    // Other positions should be false
    Rotor("VI", 'A', 'C').previousCarry shouldBe false

