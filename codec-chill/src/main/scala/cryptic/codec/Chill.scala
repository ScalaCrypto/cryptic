package cryptic
package codec

import com.twitter.chill.{KryoPool, ScalaKryoInstantiator}

import scala.util.Try

/** Object Chill provides Kryo codec and decodec functionality.
  * It contains a Kryo codec pool and given methods for codec
  * and decodec.
  *
  * The codec method creates a new instance of Codec[V].
  *
  * The encode method converts a value of type V to a PlainText using Kryo
  * codec.
  *
  * The decode method converts a PlainText back to a value of type V using
  * Kryo decodec.
  */
object Chill:
  private val kryoPool =
    KryoPool.withByteArrayOutputStream(10, new ScalaKryoInstantiator())

  given codec[V]: Codec[V] = new Codec[V]:
    def encode(value: V): PlainText = PlainText(kryoPool.toBytesWithClass(value))
    def decode(plainText: PlainText): Try[V] = Try:
      kryoPool.fromBytes(plainText.bytes).asInstanceOf[V]
