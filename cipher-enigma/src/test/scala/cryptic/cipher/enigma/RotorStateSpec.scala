package cryptic.cipher.enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RotorStateSpec extends AnyFlatSpec with Matchers:
  behavior of "RotorState"

  private val rotorStateA = RotorState(Rotor("I"), Glyph(0), 'A'.glyph)
  private val rotorStateB = RotorState(Rotor("I"), Glyph(0), 'B'.glyph)
  private val rotorStateC = RotorState(Rotor("I"), Glyph(0), 'C'.glyph)
  private val rotorStateQ = RotorState(Rotor("I"), Glyph(0), 'Q'.glyph)

  "Rotor I, ring 0, pos A" should "have a nice toString" in:
    rotorStateA.toString shouldBe "RotorState(I, 00, A)"

  it should "map inputs" in:
    rotorStateA.in('A') shouldBe 'E'
    rotorStateA.in('E') shouldBe 'L'
    rotorStateA.in('B') shouldBe 'K'
    rotorStateA.in('J') shouldBe 'Z'

  it should "map outputs" in:
    rotorStateA.out('E') shouldBe 'A'
    rotorStateA.out('L') shouldBe 'E'
    rotorStateA.out('K') shouldBe 'B'
    rotorStateA.out('Z') shouldBe 'J'

  it should "have carry = false" in:
    rotorStateA.carry shouldBe false

  it should "rotate" in:
    rotorStateA.rotate shouldBe rotorStateB
    rotorStateB.rotate shouldBe rotorStateC

  "Rotor I pos B" should "map in" in:
    rotorStateB.in('A') shouldBe 'J'
    rotorStateB.in('B') shouldBe 'L'
    rotorStateB.in('C') shouldBe 'E'
    rotorStateB.in('J') shouldBe 'M'
    rotorStateB.in('Z') shouldBe 'D'
    rotorStateB.in('V') shouldBe 'A'

  it should "map out" in:
    rotorStateB.out('J') shouldBe 'A'
    rotorStateB.out('L') shouldBe 'B'
    rotorStateB.out('E') shouldBe 'C'
    rotorStateB.out('M') shouldBe 'J'
    rotorStateB.out('D') shouldBe 'Z'
    rotorStateB.out('A') shouldBe 'V'

  "Rotor I pos C" should "map in" in:
    rotorStateC.in('A') shouldBe 'K'
    rotorStateC.in('B') shouldBe 'D'
    rotorStateC.in('R') shouldBe 'N'
    rotorStateC.in('S') shouldBe 'Y'
    rotorStateC.in('Y') shouldBe 'C'
    rotorStateC.in('Z') shouldBe 'I'

  it should "map out" in:
    rotorStateC.out('K') shouldBe 'A'
    rotorStateC.out('D') shouldBe 'B'
    rotorStateC.out('N') shouldBe 'R'
    rotorStateC.out('I') shouldBe 'Z'
    rotorStateC.out('Y') shouldBe 'S'

  "Rotor I, ring 0, pos Q" should "map input B to K" in:
    rotorStateQ.in('B') shouldBe 'E'

  "Rotor I" should "rotate and carry" in:
    val nextState = rotorStateQ.rotate
    nextState shouldBe RotorState(Rotor("I"), Glyph(0), 'R'.glyph)
    nextState.carry shouldBe true

  "Rotor VI" should "have double notch" in:
    RotorState(Rotor("VI"), Glyph(0), 'A'.glyph).carry shouldBe true
    RotorState(Rotor("VI"), Glyph(0), 'N'.glyph).carry shouldBe true
    RotorState(Rotor("VI"), Glyph(0), 'Z'.glyph).carry shouldBe false
    RotorState(Rotor("VI"), Glyph(0), 'B'.glyph).carry shouldBe false
    RotorState(Rotor("VI"), Glyph(0), 'M'.glyph).carry shouldBe false
    RotorState(Rotor("VI"), Glyph(0), 'O'.glyph).carry shouldBe false
