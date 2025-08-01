package cryptic

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Success

class SerializerSpec extends AnyFlatSpec with Matchers:

  val bytes: Array[Byte] = Array[Byte](0, 1, 2, 3, 4, 5, 6)
  val text = "kalle"
  val int = 17
  val long = 4711L
  val double: Double = math.E
  val bytesPT: PlainText = PlainText(bytes)
  val textPT: PlainText = PlainText(text)
  val intPT: PlainText = PlainText(int)
  val longPT: PlainText = PlainText(long)
  val doublePT: PlainText = PlainText(double)
  "Serializer" should "serialize to PlainText" in:
    bytesSerializer.serialize(bytes) shouldEqual bytesPT
    stringSerializer.serialize(text) shouldEqual textPT
    intSerializer.serialize(int) shouldEqual intPT
    longSerializer.serialize(long) shouldEqual longPT
    doubleSerializer.serialize(double) shouldEqual doublePT

  "Serializer" should "deserialize to value" in:
    bytesSerializer.deserialize(bytesPT) shouldEqual Success(bytes)
    stringSerializer.deserialize(textPT) shouldEqual Success(text)
    intSerializer.deserialize(intPT) shouldEqual Success(int)
    longSerializer.deserialize(longPT) shouldEqual Success(long)
    doubleSerializer.deserialize(doublePT) shouldEqual Success(double)
