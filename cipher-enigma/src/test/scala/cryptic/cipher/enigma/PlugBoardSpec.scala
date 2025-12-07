package cryptic
package cipher
package enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class PlugBoardSpec
    extends AnyFlatSpec
    with Matchers
    with TableDrivenPropertyChecks:
  behavior of "PlugBoard"

  it should "swap mapped glyphs and leave others unchanged" in:
    val pb = PlugBoard("ABCD") // A<->B, C<->D
    val data = Table(
      ("in", "out"),
      ('A', 'B'),
      ('B', 'A'),
      ('C', 'D'),
      ('D', 'C'),
      ('Z', 'Z') // unaffected
    )
    forAll(data): (in: Char, out: Char) =>
      pb.swap(in.glyph) shouldBe out.glyph

  it should "support empty wiring (no-op)" in:
    val pb = PlugBoard("")
    pb.swap('A'.glyph) shouldBe 'A'.glyph
    pb.swap('Z'.glyph) shouldBe 'Z'.glyph

  it should "normalize lowercase input when constructing from String" in:
    val pb = PlugBoard("abCd")
    // toString uses uppercase glyphs
    pb.toString shouldBe "ABCD"
    // swap behavior operates on Glyphs; use uppercase inputs
    pb.swap('A'.glyph) shouldBe 'B'.glyph
    pb.swap('C'.glyph) shouldBe 'D'.glyph

  behavior of "PlugBoard.apply(pairs: String)"

  it should "reject non-letter characters" in:
    intercept[IllegalArgumentException] { PlugBoard("AB12") }
    intercept[IllegalArgumentException] { PlugBoard("--__ ") }

  it should "reject an odd number of letters" in:
    intercept[IllegalArgumentException] { PlugBoard("ABC") }

  it should "reject more than 10 pairs (20 letters)" in:
    intercept[IllegalArgumentException] { PlugBoard("ABCDEFGHIJKLMNOPQRSTUVWXYZ") }

  it should "reject self-pairs and duplicate letters across pairs" in:
    // self-pair AA
    intercept[IllegalArgumentException] { PlugBoard("AA") }
    // duplicate A used in two pairs
    intercept[IllegalArgumentException] { PlugBoard("ABAC") }

  it should "preserve pair order and render as concatenated pairs in toString" in:
    PlugBoard("ABCD").toString shouldBe "ABCD"
    PlugBoard("EFGH").toString shouldBe "EFGH"

  it should "construct an empty PlugBoard from an empty string" in:
     PlugBoard("").wiring.isEmpty shouldBe true
