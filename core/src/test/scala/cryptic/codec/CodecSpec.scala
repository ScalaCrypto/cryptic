package cryptic
package codec

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Success, Try}

class CodecSpec extends AnyFlatSpec with Matchers:
  import cryptic.codec.default.{*, given}
  val bytes: Array[Byte] = Array[Byte](0, 1, 2, 3, 4, 5, 6)
  val text = "kalle"
  val int = 17
  val long = 4711L
  val double: Double = math.E
  val float: Float = double.toFloat
  val bytesPT: PlainText = PlainText(bytes)
  val textPT: PlainText = PlainText(text)
  val intPT: PlainText = PlainText(int)
  val longPT: PlainText = PlainText(long)
  val floatPT: PlainText = PlainText(float)
  val doublePT: PlainText = PlainText(double)
  "Codec" should "encode simple types to plain text" in:
    bytes.encoded shouldEqual bytesPT
    text.encoded shouldEqual textPT
    int.encoded shouldEqual intPT
    long.encoded shouldEqual longPT
    float.encoded shouldEqual floatPT
    double.encoded shouldEqual doublePT

  "Codec" should "decode cypher text to simple types" in:
    def decode[V: Codec](pt: PlainText): Try[V] = (pt.decoded): Try[V]
    decode[Array[Byte]](bytesPT) shouldEqual Success(bytes)
    decode[String](textPT) shouldEqual Success(text)
    decode[Int](intPT) shouldEqual Success(int)
    decode[Long](longPT) shouldEqual Success(long)
    decode[Float](floatPT) shouldEqual Success(float)
    decode[Double](doublePT) shouldEqual Success(double)
