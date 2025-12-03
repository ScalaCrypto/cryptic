package cryptic
package cipher
package enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RotorApplySpec extends AnyFlatSpec with Matchers:
  behavior of "Rotor.apply(settings: String) — dedicated parsing tests"

  private val formatError = "Rotor.apply requires format \"wheel ring pos\" with ring/pos as letters A-Z or a-z"

  it should "accept leading and trailing whitespace" in:
    Rotor("   I A A   ") shouldBe Rotor("I", 'A', 'A')

  it should "accept tab separators as whitespace" in:
    // Tabs (\t) are matched by \s in the regex
    Rotor("II\tB\tC") shouldBe Rotor("II", 'B', 'C')

  it should "normalize lowercase ring/pos to uppercase" in:
    Rotor("III a z") shouldBe Rotor("III", 'A', 'Z')
    Rotor("IV m n") shouldBe Rotor("IV", 'M', 'N')

  it should "handle mixed spacing between tokens" in:
    Rotor("V    A\t\tZ") shouldBe Rotor("V", 'A', 'Z')

  it should "reject empty or malformed formats with a clear message" in:
    val ex1 = intercept[IllegalArgumentException] { Rotor("") }
    ex1.getMessage shouldBe formatError

    val ex2 = intercept[IllegalArgumentException] { Rotor("I A") }
    ex2.getMessage shouldBe formatError

    val ex3 = intercept[IllegalArgumentException] { Rotor("I A A X") }
    ex3.getMessage shouldBe formatError

  it should "reject non-letter ring/pos with the format error message" in:
    val ex1 = intercept[IllegalArgumentException] { Rotor("I 1 A") }
    ex1.getMessage shouldBe formatError

    val ex2 = intercept[IllegalArgumentException] { Rotor("I A 1") }
    ex2.getMessage shouldBe formatError

    val ex3 = intercept[IllegalArgumentException] { Rotor("I å A") }
    ex3.getMessage shouldBe formatError

  it should "reject unknown wheel names via Wheel" in:
    intercept[IllegalArgumentException] { Rotor("UNKNOWN A A") }
