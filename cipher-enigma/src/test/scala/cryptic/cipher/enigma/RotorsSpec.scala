package cryptic
package cipher
package enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class RotorsSpec
    extends AnyFlatSpec
    with Matchers
    with TableDrivenPropertyChecks:
  behavior of "Rotors.rotate"

  it should "rotate 3 rotors" in:
    val data = Table(
      // Order is right to left, where left is at index 0
      // We do to rotations to check for the double step anomaly
      // of the middle rotor
      ("Before", "First", "Second"),
      ("JAA", "JAB", "JAC"), // Only left rotor
      ("KAQ", "KBR", "KBS"), // left R carry => middle rotate
      ("LEQ", "MFR", "MFS"), // All three
      ("MDQ", "MER", "NFS"), // Double step anomaly
      ("NER", "OFS", "OFT") // Double step anomaly
    )
    forAll(data): (before: String, first: String, second: String) =>
      val rotate1 = Rotors(s"III-II-I AAA $before").rotate
      withClue(s"Failed after 1st rotation starting from $before: ") {
        rotate1.pos.string.reverse shouldBe first
      }
      val rotate2 = rotate1.rotate
      withClue(s"Failed after 2nd rotation starting from ${rotate1.pos.string.reverse}: ") {
        rotate2.pos.string.reverse shouldBe second
      }

  it should "rotate the right rotor only when no carry occurs (3 rotors)" in:
    val rotors = Rotors("III-II-I AAA AAA").rotate
    rotors.pos.string.reverse shouldBe "AAB"

  it should "rotate middle when right carries after its rotation (3 rotors)" in:
    val rotors = Rotors("III-II-I AAA AAQ").rotate
    rotors.pos.string.reverse shouldBe "ABR"

  it should "rotate left when middle carries after its rotation (double step trigger, 3 rotors)" in:
    val rotors = Rotors("III-II-I AAA AEQ").rotate
    rotors.pos.string.reverse shouldBe "BFR"

  it should "work with a single rotor (always rotates)" in:
    val single0 = Rotors(IArray(Rotor("I A A")))
    val single1 = single0.rotate
    single1.rotors.length shouldBe 1
    single1.rotors(0).pos shouldBe 'B'.glyph

  it should "ripple carry across four rotors" in:
    // Set up a chain where first rotation causes cascading carries:
    // Right (Rotor I) at Q -> rotates to R and carries (Rotor I notch R)
    // Next (Rotor II) at E -> when rotated to F, carries (Rotor II notch F)
    // Next (Rotor III) at V -> when rotated to W, carries (Rotor III notch W)
    // Leftmost (Rotor IV) at A -> should rotate to B due to carry chain
    val r0 = Rotor("I A Q")
    val r1 = Rotor("II A E")
    val r2 = Rotor("III A V")
    val r3 = Rotor("IV A A")
    val rotors0 = Rotors(IArray(r0, r1, r2, r3))
    val rotors1 = rotors0.rotate
    rotors1.rotors
      .map(_.pos) shouldBe IArray('R'.glyph, 'F'.glyph, 'W'.glyph, 'B'.glyph)

  behavior of "Rotors constructor"

  it should "require at least one rotor state and accept arbitrary sizes" in:
    // empty should fail
    intercept[IllegalArgumentException] {
      Rotors(IArray.empty[Rotor])
    }
    // 1, 2, 3, 4 should all be allowed
    noException should be thrownBy Rotors(IArray(Rotor("I A A")))
    noException should be thrownBy Rotors(
      IArray(Rotor("I A A"), Rotor("II A A"))
    )
    noException should be thrownBy Rotors(
      IArray(Rotor("I A A"), Rotor("II A A"), Rotor("III A A"))
    )
    noException should be thrownBy Rotors(
      IArray(Rotor("I A A"), Rotor("II A A"), Rotor("III A A"), Rotor("IV A A"))
    )

  it should "reject duplicate rotors (by identity/name)" in:
    // Duplicate rotor I used twice should fail
    intercept[IllegalArgumentException] {
      Rotors(IArray(Rotor("I A A"), Rotor("I A B")))
    }
    // Duplicates among more than two should also fail
    intercept[IllegalArgumentException] {
      Rotors(IArray(Rotor("I A A"), Rotor("II A A"), Rotor("I A C")))
    }

  behavior of "Rotors.in"

  it should "chain RotorState.in from right-most to left-most (3 rotors)" in:
    val r0 = Rotor("I A A")
    val r1 = Rotor("II A A")
    val r2 = Rotor("III A A")
    val rotors = Rotors(IArray(r0, r1, r2))

    val input = 'A'
    val step1 = r0.in(input)
    val step2 = r1.in(step1)
    val step3 = r2.in(step2)

    rotors.in(input) shouldBe step3

  it should "match single rotor behavior and support char overload" in:
    val single = Rotors(IArray(Rotor("I A A")))
    val g = 'C'.glyph

    single.in(g) shouldBe (single.rotors(0).in(g), List(2, 12))
    single.in('C') shouldBe single.in(g.char)

  behavior of "Rotors companion object apply"

  it should "construct from varargs with at least one state (single)" in:
    val single = Rotors(Rotor("I A A"))
    single.rotors.length shouldBe 1
    single.rotors(0).pos shouldBe 'A'.glyph

  it should "construct from varargs with multiple states and preserve order" in:
    val r0 = Rotor("I A A")
    val r1 = Rotor("II A B")
    val r2 = Rotor("III A C")
    val rotors = Rotors(r0, r1, r2)
    rotors.rotors.length shouldBe 3
    rotors.rotors(0) shouldBe r0
    rotors.rotors(1) shouldBe r1
    rotors.rotors(2) shouldBe r2


  it should "reject duplicates when constructed via varargs" in:
    intercept[IllegalArgumentException] {
      Rotors(Rotor("I A A"), Rotor("I A B"))
    }

  behavior of "Rotors.out"

  it should "chain RotorState.out from left-most to right-most (3 rotors)" in:
    val r0 = Rotor("I A A")
    val r1 = Rotor("II A A")
    val r2 = Rotor("III A A")
    val rotors = Rotors(IArray(r0, r1, r2))

    val input = 'Z'
    // Out path is left-most to right-most: r2, then r1, then r0
    val step1 = r2.out(input)
    val step2 = r1.out(step1)
    val step3 = r0.out(step2)

    rotors.out(input) shouldBe step3

  it should "match single rotor behavior and support char overload" in:
    val single = Rotors(IArray(Rotor("I A A")))
    val g = 'M'.glyph

    single.out(g) shouldBe (single.rotors(0).out(g), List(12, 2))
    single.out('M') shouldBe single.out(g.char)
