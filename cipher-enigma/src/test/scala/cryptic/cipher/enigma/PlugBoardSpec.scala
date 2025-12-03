package cryptic
package cipher
package enigma

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlugBoardSpec extends AnyFlatSpec with Matchers:
  behavior of "PlugBoard"

  it should "swap pairs and be identity for others" in:
    val pb = PlugBoard("ABCD") // A<->B, C<->D
    pb.swap('A'.glyph).char shouldBe 'B'
    pb.swap('B'.glyph).char shouldBe 'A'
    pb.swap('C'.glyph).char shouldBe 'D'
    pb.swap('D'.glyph).char shouldBe 'C'
    // Unplugged letters unchanged
    pb.swap('E'.glyph).char shouldBe 'E'

  it should "be symmetric (swap twice yields original)" in:
    val pb = PlugBoard("QWERTY") // Q<->W, E<->R, T<->Y
    ('A' to 'Z').foreach: c =>
      val g = c.glyph
      pb.swap(pb.swap(g)) shouldBe g

  it should "accept empty wiring and act as identity" in:
    val pb = PlugBoard("")
    ('A' to 'Z').foreach: c =>
      pb.swap(c.glyph).char shouldBe c

  it should "parse lowercase and uppercase letters equivalently" in:
    val up = PlugBoard("ABCD")
    val lo = PlugBoard("abCd")
    ('A' to 'Z').foreach: c =>
      up.swap(c.glyph) shouldBe lo.swap(c.glyph)

  it should "reject odd number of letters" in:
    an[IllegalArgumentException] should be thrownBy PlugBoard("ABC")

  it should "reject duplicate letters across pairs" in:
    // 'A' duplicated
    an[IllegalArgumentException] should be thrownBy PlugBoard("ABAC")

  it should "reject self-pairs like AA" in:
    an[IllegalArgumentException] should be thrownBy PlugBoard("AABC")

  it should "reject more than 10 pairs" in:
    val twentyTwo = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".take(22) // 11 pairs
    an[IllegalArgumentException] should be thrownBy PlugBoard(twentyTwo)

  it should "reject non-letter characters" in:
    an[IllegalArgumentException] should be thrownBy PlugBoard("AB- CD")
