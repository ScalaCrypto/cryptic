package cryptic
package codec

import java.nio.ByteBuffer
import scala.util.Try
import scala.util.Success

/** Built-in Codec instances for common Scala types. */
object default extends Codec.Companion:
  given optionCodec: [V: Codec] => Codec[Option[V]]:
    def encode(v: Option[V], aad: AAD): PlainText =
      v match
        case None        => PlainText.empty
        case Some(value) => summon[Codec[V]].encode(value, aad)
    def decode(pt: PlainText): Try[Option[V]] =
      if pt.bytes.isEmpty then Success(None)
      else summon[Codec[V]].decode(pt).map(Some(_))

  given Codec[IArray[Byte]]:
    def encode(v: IArray[Byte], aad: AAD): PlainText =
      PlainText(v, aad)
    def decode(pt: PlainText): Try[IArray[Byte]] =
      Success(pt.bytes)

  given Codec[String]:
    def encode(v: String, aad: AAD): PlainText = PlainText(v, aad)
    def decode(pt: PlainText): Try[String] = Try(pt.bytes.string)

  given Codec[Boolean]:
    def encode(v: Boolean, aad: AAD): PlainText =
      val b: Byte = if v then 1.toByte else 0.toByte
      PlainText(IArray.unsafeFromArray(Array(b)), aad)
    def decode(pt: PlainText): Try[Boolean] = Try(
      pt.bytes.nonEmpty && pt.bytes(0) != 0
    )

  given Codec[Int]:
    def encode(v: Int, aad: AAD): PlainText = PlainText(v.bytes, aad)
    def decode(pt: PlainText): Try[Int] = Try(pt.bytes.int)

  given Codec[Long]:
    def encode(v: Long, aad: AAD): PlainText = PlainText(v.bytes, aad)
    def decode(pt: PlainText): Try[Long] = Try(pt.bytes.long)

  given Codec[Float]:
    def encode(v: Float, aad: AAD): PlainText = PlainText(v.bytes, aad)
    def decode(pt: PlainText): Try[Float] = Try(pt.bytes.float)

  given Codec[Double]:
    def encode(v: Double, aad: AAD): PlainText = PlainText(v.bytes, aad)
    def decode(pt: PlainText): Try[Double] = Try(pt.bytes.double)
