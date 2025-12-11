package cryptic
package cipher
package enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RotorsApplySpec extends AnyFlatSpec with Matchers:
  behavior of "Rotors.apply(settings: String)"

  private val formatError =
    "Rotors.apply requires format \"names rings positions\" (e.g. \"VI-II-I ABC DEF\") with ring/pos as letters A-Z or a-z"

  it should "parse the example 'VI-II-I ABC DEF' into right-most-first order" in:
    val r = Rotors("VI-II-I ABC DEF")
    val expected = Rotors(Rotor("I C F"), Rotor("II B E"), Rotor("VI A D"))
    r.rotors.toSeq shouldBe expected.rotors.toSeq

  it should "accept arbitrary rotor counts and normalize lowercase ring/pos" in:
    val r = Rotors("IV-III-II-I abcd efgh")
    r.rotors.length shouldBe 4
    // Expected right-most-first
    val expected = Rotors(
      Rotor("I D H"),
      Rotor("II C G"),
      Rotor("III B F"),
      Rotor("IV A E")
    )
    r.rotors.toSeq shouldBe expected.rotors.toSeq

  it should "tolerate leading/trailing whitespace and mixed internal spacing" in:
    val r1 = Rotors("   VI-II-I   ABC   DEF   ")
    val r2 = Rotors("VI-II-I\tABC\tDEF")
    val expected = Rotors(Rotor("I C F"), Rotor("II B E"), Rotor("VI A D"))
    r1.rotors.toSeq shouldBe expected.rotors.toSeq
    r2.rotors.toSeq shouldBe expected.rotors.toSeq

  it should "reject mismatched lengths with a clear message" in:
    val ex = intercept[IllegalArgumentException] { Rotors("I-II ABC DE") }
    ex.getMessage shouldBe "Rotors.apply rings and positions must have length 2 to match names count"

  it should "reject malformed formats with the format error message" in:
    val ex1 = intercept[IllegalArgumentException] { Rotors("") }
    ex1.getMessage shouldBe formatError

    val ex2 = intercept[IllegalArgumentException] { Rotors("I-II ABC") }
    ex2.getMessage shouldBe formatError

  it should "reject non-letter ring/pos with the format error message" in:
    val ex1 = intercept[IllegalArgumentException] { Rotors("I 1 A") }
    ex1.getMessage shouldBe formatError
    val ex2 = intercept[IllegalArgumentException] { Rotors("I A 1") }
    ex2.getMessage shouldBe formatError

  it should "reject unknown wheel names via Wheel" in:
    intercept[IllegalArgumentException] {
      Rotors("UNKNOWN-I AB CD")
    }
