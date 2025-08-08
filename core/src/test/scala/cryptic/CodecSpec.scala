package cryptic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class CodecSpec extends AnyFlatSpec with Matchers:
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
    summon[Codec[Array[Byte]]].encode(bytes) shouldEqual bytesPT
    summon[Codec[String]].encode(text) shouldEqual textPT
    summon[Codec[Int]].encode(int) shouldEqual intPT
    summon[Codec[Long]].encode(long) shouldEqual longPT
    summon[Codec[Float]].encode(float) shouldEqual floatPT
    summon[Codec[Double]].encode(double) shouldEqual doublePT

  "Codec" should "decode cypher text to simple types" in:
    summon[Codec[Array[Byte]]].decode(bytesPT) shouldEqual Success(bytes)
    summon[Codec[String]].decode(textPT) shouldEqual Success(text)
    summon[Codec[Int]].decode(intPT) shouldEqual Success(int)
    summon[Codec[Long]].decode(longPT) shouldEqual Success(long)
    summon[Codec[Float]].decode(floatPT) shouldEqual Success(float)
    summon[Codec[Double]].decode(doublePT) shouldEqual Success(double)
