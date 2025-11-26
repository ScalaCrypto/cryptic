package cryptic.cipher.enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RotorsSpec extends AnyFlatSpec with Matchers:
  behavior of "Rotors.rotate"

  it should "rotate the right rotor only when no carry occurs (3 rotors)" in:
    val right0  = RotorState("I", 0, 'A')   // Rotor I: will move to B, no carry (notch = R)
    val middle0 = RotorState("II", 0, 'A')  // Different rotor to satisfy uniqueness
    val left0   = RotorState("III", 0, 'A')
    val rotors0 = Rotors(IArray(right0, middle0, left0))

    val rotors1 = rotors0.rotate

    rotors1.states(0).pos shouldBe 'B'.glyph // right advanced
    rotors1.states(1).pos shouldBe 'A'.glyph // middle unchanged
    rotors1.states(2).pos shouldBe 'A'.glyph // left unchanged

  it should "rotate middle when right carries after its rotation (3 rotors)" in:
    val right0  = RotorState("I", 0, 'Q')   // rotate -> R, which carries for Rotor I
    val middle0 = RotorState("II", 0, 'A')  // will rotate to B (no carry for Rotor II)
    val left0   = RotorState("III", 0, 'A')
    val rotors0 = Rotors(IArray(right0, middle0, left0))

    val rotors1 = rotors0.rotate

    rotors1.states(0).pos shouldBe 'R'.glyph // right advanced and at notch
    rotors1.states(1).pos shouldBe 'B'.glyph // middle advanced due to carry
    rotors1.states(2).pos shouldBe 'A'.glyph // left unchanged (no carry from middle yet)

  it should "rotate left when middle carries after its rotation (double step trigger, 3 rotors)" in:
    val right0  = RotorState("I", 0, 'Q')    // rotate -> R and carries, causing middle rotate
    val middle0 = RotorState("II", 0, 'E')   // when rotated (due to right carry) -> F (Rotor II notch) and carries
    val left0   = RotorState("III", 0, 'A')
    val rotors0 = Rotors(IArray(right0, middle0, left0))

    val rotors1 = rotors0.rotate

    rotors1.states(0).pos shouldBe 'R'.glyph // right advanced
    rotors1.states(1).pos shouldBe 'F'.glyph // middle advanced to notch (Rotor II)
    rotors1.states(2).pos shouldBe 'B'.glyph // left advanced due to middle carry

  it should "work with a single rotor (always rotates)" in:
    val single0 = Rotors(IArray(RotorState("I", 0, 'A')))
    val single1 = single0.rotate
    single1.states.length shouldBe 1
    single1.states(0).pos shouldBe 'B'.glyph

  it should "ripple carry across four rotors" in:
    // Set up a chain where first rotation causes cascading carries:
    // Right (Rotor I) at Q -> rotates to R and carries (Rotor I notch R)
    // Next (Rotor II) at E -> when rotated to F, carries (Rotor II notch F)
    // Next (Rotor III) at V -> when rotated to W, carries (Rotor III notch W)
    // Leftmost (Rotor IV) at A -> should rotate to B due to carry chain
    val r0 = RotorState("I", 0, 'Q')
    val r1 = RotorState("II", 0, 'E')
    val r2 = RotorState("III", 0, 'V')
    val r3 = RotorState("IV", 0, 'A')
    val rotors0 = Rotors(IArray(r0, r1, r2, r3))
    val rotors1 = rotors0.rotate
    rotors1.states.map(_.pos) shouldBe IArray('R'.glyph, 'F'.glyph, 'W'.glyph, 'B'.glyph)

  behavior of "Rotors constructor"

  it should "require at least one rotor state and accept arbitrary sizes" in:
    // empty should fail
    intercept[IllegalArgumentException] {
      Rotors(IArray.empty[RotorState])
    }
    // 1, 2, 3, 4 should all be allowed
    noException should be thrownBy Rotors(IArray(RotorState("I", 0, 'A')))
    noException should be thrownBy Rotors(IArray(RotorState("I", 0, 'A'), RotorState("II", 0, 'A')))
    noException should be thrownBy Rotors(IArray(RotorState("I", 0, 'A'), RotorState("II", 0, 'A'), RotorState("III", 0, 'A')))
    noException should be thrownBy Rotors(IArray(RotorState("I", 0, 'A'), RotorState("II", 0, 'A'), RotorState("III", 0, 'A'), RotorState("IV", 0, 'A')))

  it should "reject duplicate rotors (by identity/name)" in:
    // Duplicate rotor I used twice should fail
    intercept[IllegalArgumentException] {
      Rotors(IArray(RotorState("I", 0, 'A'), RotorState("I", 0, 'B')))
    }
    // Duplicates among more than two should also fail
    intercept[IllegalArgumentException] {
      Rotors(IArray(RotorState("I", 0, 'A'), RotorState("II", 0, 'A'), RotorState("I", 0, 'C')))
    }

  behavior of "Rotors.in"

  it should "chain RotorState.in from right-most to left-most (3 rotors)" in:
    val r0 = RotorState("I", 0, 'A')
    val r1 = RotorState("II", 0, 'A')
    val r2 = RotorState("III", 0, 'A')
    val rotors = Rotors(IArray(r0, r1, r2))

    val input = 'A'.glyph
    val step1 = r0.in(input)
    val step2 = r1.in(step1)
    val step3 = r2.in(step2)

    rotors.in(input) shouldBe step3

  it should "match single rotor behavior and support char overload" in:
    val single = Rotors(IArray(RotorState("I", 0, 'A')))
    val g = 'C'.glyph

    single.in(g) shouldBe single.states(0).in(g)
    single.in('C') shouldBe single.in(g).char

  behavior of "Rotors companion object apply"

  it should "construct from varargs with at least one state (single)" in:
    val single = Rotors(RotorState("I", 0, 'A'))
    single.states.length shouldBe 1
    single.states(0).pos shouldBe 'A'.glyph

  it should "construct from varargs with multiple states and preserve order" in:
    val r0 = RotorState("I", 0, 'A')
    val r1 = RotorState("II", 0, 'B')
    val r2 = RotorState("III", 0, 'C')
    val rotors = Rotors(r0, r1, r2)
    rotors.states.length shouldBe 3
    rotors.states(0) shouldBe r0
    rotors.states(1) shouldBe r1
    rotors.states(2) shouldBe r2

  it should "reject duplicates when constructed via varargs" in:
    intercept[IllegalArgumentException] {
      Rotors(RotorState("I", 0, 'A'), RotorState("I", 0, 'B'))
    }

  behavior of "Rotors.out"

  it should "chain RotorState.out from left-most to right-most (3 rotors)" in:
    val r0 = RotorState("I", 0, 'A')
    val r1 = RotorState("II", 0, 'A')
    val r2 = RotorState("III", 0, 'A')
    val rotors = Rotors(IArray(r0, r1, r2))

    val input = 'Z'.glyph
    // Out path is left-most to right-most: r2, then r1, then r0
    val step1 = r2.out(input)
    val step2 = r1.out(step1)
    val step3 = r0.out(step2)

    rotors.out(input) shouldBe step3

  it should "match single rotor behavior and support char overload" in:
    val single = Rotors(IArray(RotorState("I", 0, 'A')))
    val g = 'M'.glyph

    single.out(g) shouldBe single.states(0).out(g)
    single.out('M') shouldBe single.out(g).char
