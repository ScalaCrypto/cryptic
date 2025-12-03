package cryptic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class AADParameterizedSpec
    extends AnyFlatSpec
    with Matchers
    with TableDrivenPropertyChecks:

  "AAD extensions" should "correctly round-trip different data types" in:
    val stringData = Table(
      ("input", "expectedLength"), // Headers
      ("hello", 5),
      ("secret payload", 14),
      ("", 0)
    )

    forAll(stringData)((input: String, expectedLength: Int) =>
      val aad = input.aad

      aad.string shouldBe input
      aad.bytes.length shouldBe expectedLength
    )

  it should "correctly round-trip numeric types" in:
    val intData = List(1, 0, -999, Int.MaxValue)

    for number <- intData do number.aad.int shouldBe number
