package cryptic
package codec

import java.nio.ByteBuffer
import scala.util.Try
import scala.util.Success

object default extends Codec.Companion:
  given optionCodec: [V: Codec] => Codec[Option[V]]:
    def encode(v: Option[V]): PlainText =
      v match
        case Some(value) => summon[Codec[V]].encode(value)
        case None        => PlainText.empty
    def decode(pt: PlainText): Try[Option[V]] =
      if pt.bytes.isEmpty then Success(None)
      else summon[Codec[V]].decode(pt).map(Some(_))

  given Codec[IArray[Byte]]:
    def encode(v: IArray[Byte]): PlainText = PlainText(v)
    def decode(pt: PlainText): Try[IArray[Byte]] =
      Success(pt.bytes)

  given Codec[String]:
    def encode(v: String): PlainText = PlainText(v)
    def decode(pt: PlainText): Try[String] =
      Success(new String(pt.bytes.mutable))

  given Codec[Int]:
    def encode(v: Int): PlainText =
      val buffer = ByteBuffer.allocate(4)
      buffer.putInt(v)
      PlainText(buffer.array().immutable)
    def decode(pt: PlainText): Try[Int] =
      Try(ByteBuffer.wrap(pt.bytes.mutable).getInt)

  given Codec[Long]:
    def encode(v: Long): PlainText =
      val buffer = ByteBuffer.allocate(8)
      buffer.putLong(v)
      PlainText(buffer.array().immutable)
    def decode(pt: PlainText): Try[Long] =
      Try(ByteBuffer.wrap(pt.bytes.mutable).getLong)

  given Codec[Float]:
    def encode(v: Float): PlainText =
      val buffer = ByteBuffer.allocate(4)
      buffer.putFloat(v)
      PlainText(buffer.array().immutable)
    def decode(pt: PlainText): Try[Float] =
      Try(ByteBuffer.wrap(pt.bytes.mutable).getFloat)

  given Codec[Double]:
    def encode(v: Double): PlainText =
      val buffer = ByteBuffer.allocate(8)
      buffer.putDouble(v)
      PlainText(buffer.array().immutable)
    def decode(pt: PlainText): Try[Double] =
      Try(ByteBuffer.wrap(pt.bytes.mutable).getDouble)
