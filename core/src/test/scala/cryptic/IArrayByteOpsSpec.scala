package cryptic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IArrayByteOpsSpec extends AnyFlatSpec with Matchers:
  "IArray[Byte] xor" should "XOR bytes pairwise" in:
    val left = IArray(0x0f, 0xf0, 0xaa).map(_.toByte)
    val right = IArray(0xf0, 0x0f, 0x55).map(_.toByte)

    left.xor(right) shouldEqual IArray(0xff, 0xff, 0xff).map(_.toByte)

  it should "fail when byte arrays have different lengths" in:
    val left = IArray(0x01, 0x02).map(_.toByte)
    val right = IArray(0x01).map(_.toByte)

    intercept[IllegalArgumentException]:
      left.xor(right)
