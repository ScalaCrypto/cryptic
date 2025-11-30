package cryptic.cipher.enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class RotorSpec
    extends AnyFlatSpec
    with Matchers
    with TableDrivenPropertyChecks:
  behavior of "Rotor"

  "Rotor" should "map and carry after rotate" in:
    val data = Table(
      ("Settings", "plain", "cipher", "carry"),
      ("I A Z", 'A', 'E', false),
      ("I A Z", 'B', 'K', false),
      ("I A Z", 'E', 'L', false),
      ("I A Z", 'J', 'Z', false),
      ("I A Z", 'S', 'S', false),
      ("I A A", 'A', 'J', false),
      ("I A A", 'B', 'L', false),
      ("I A B", 'A', 'K', false),
      ("I A B", 'B', 'D', false),
      ("I B Z", 'A', 'K', false),
      ("I B B", 'A', 'J', false),
      ("I B Q", 'A', 'H', true),
      ("I Z Q", 'A', 'A', true),
      ("I A Q", 'A', 'D', true),
      ("I B R", 'A', 'D', false),
      ("I C S", 'A', 'D', false),
      ("I Z P", 'A', 'D', false),
      ("I Y O", 'A', 'D', false),
      ("I B Q", 'M', 'W', true),
      ("I B Q", 'A', 'H', true),
      ("II A Z", 'D', 'K', false),
      ("III A Z", 'K', 'X', false)
    )
    forAll(data):
      (settings: String, plain: Char, cipher: Char, carry: Boolean) =>
        val rotor = Rotor(settings).rotate // We rotate before mapping
        rotor.in(plain) shouldBe cipher
        rotor.out(cipher) shouldBe plain
        rotor.carry shouldBe carry

  private val rotorA = Rotor("I A A")
  private val rotorB = Rotor("I A B")
  private val rotorC = Rotor("I A C")
  private val rotorQ = Rotor("I A Q")
  private val rotorBQ = Rotor("I B Q")

  it should "rotate" in:
    val data = Table(
      ("before", "after"),
      ("I A A", "I A B"),
      ("I A B", "I A C")
    )
    forAll(data): (before, after) =>
      Rotor(before).rotate shouldBe Rotor(after)

  it should "have carry on notch" in:
    val data = Table(
      ("rotor", "carry"),
      ("I A A", false),
      ("I A Q", false),
      ("I A R", true),
      ("I B Q", false),
      ("I B R", true),
      ("I B S", false),
      ("VI A N", true),
      ("VI A Z", false),
      ("VI A B", false),
      ("VI A M", false),
      ("VI A O", false),
      ("II A A", false)
    )
    forAll(data): (rotor: String, carry: Boolean) =>
      Rotor(rotor).carry shouldBe carry

  behavior of "Rotor.apply(settings: String)"

  it should "parse \"wheel ring pos\" with spaces" in:
    Rotor("I A A") shouldBe Rotor("I", 'A', 'A')
    Rotor("II B C") shouldBe Rotor("II", 'B', 'C')

  it should "accept lowercase ring/pos and normalize" in:
    Rotor("III a z") shouldBe Rotor("III", 'A', 'Z')

  it should "tolerate extra internal whitespace" in:
    Rotor("IV   A    Z") shouldBe Rotor("IV", 'A', 'Z')

  it should "reject wrong token counts" in:
    intercept[IllegalArgumentException] { Rotor("") }
    intercept[IllegalArgumentException] { Rotor("I A") }
    intercept[IllegalArgumentException] { Rotor("I A A X") }

  it should "reject non-letter ring/pos" in:
    intercept[IllegalArgumentException] { Rotor("I 1 A") }
    intercept[IllegalArgumentException] { Rotor("I A 1") }

  it should "reject unknown wheel names via Wheel" in:
    intercept[IllegalArgumentException] { Rotor("VII A A") }
