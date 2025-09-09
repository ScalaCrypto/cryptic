package cryptic
package codec

import cryptic.support.TestBase
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Success, Try}

class CodecSpec extends TestBase:
  import cryptic.codec.default.{*, given}
  val bytes: IArray[Byte] = IArray[Byte](0, 1, 2, 3, 4, 5, 6)
  val text = "kalle"
  val int = 17
  val long = 4711L
  val double: Double = math.E
  val float: Float = double.toFloat
  "Codec" should "encode simple types to plain text" in:
    bytes.encoded.bytes shouldEqual bytes
    text.encoded.bytes shouldEqual text.getBytes.immutable
    int.encoded.bytes shouldEqual IArray[Byte](0, 0, 0, 17)
    long.encoded.bytes shouldEqual IArray(0, 0, 0, 0, 0, 0, 18, 103)
    float.encoded.bytes shouldEqual IArray(64, 45, -8, 84)
    double.encoded.bytes shouldEqual IArray(64, 5, -65, 10, -117, 20, 87, 105)

  "Codec" should "decode PlainText to specified types" in:
    def decode[V: Codec](pt: PlainText): Try[V] = pt.decoded: Try[V]
    decode[IArray[Byte]](bytes.encoded) shouldEqual Success(bytes)
    decode[String](text.encoded) shouldEqual Success(text)
    decode[Int](int.encoded) shouldEqual Success(int)
    decode[Long](long.encoded) shouldEqual Success(long)
    decode[Float](float.encoded) shouldEqual Success(float)
    decode[Double](double.encoded) shouldEqual Success(double)
