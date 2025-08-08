package cryptic
package codec

import com.twitter.chill.{KryoPool, ScalaKryoInstantiator}

import scala.util.Try

/** Object Chill provides encoding and decoding functionality using Kryo
  * serialization. It contains a Kryo pool and given methods for encoding and
  * decoding.
  *
  * The codec method creates a new instance of Codec[V].
  *
  * The encode method converts a value of type V to a PlainText using Kryo
  * serialization.
  *
  * The decode method converts a PlainText back to a value of type V using Kryo
  * serialization.
  */
object Chill extends Codec.Companion:
  private val kryoPool =
    KryoPool.withByteArrayOutputStream(10, new ScalaKryoInstantiator())

  given codec: [V] => Codec[V]:
    def encode(v: V): PlainText = PlainText(kryoPool.toBytesWithClass(v))
    def decode(pt: PlainText): Try[V] = Try(
      kryoPool.fromBytes(pt.bytes).asInstanceOf[V]
    )
