package cryptic
package serialization

import com.twitter.chill.{ KryoPool, ScalaKryoInstantiator }

import scala.util.Try

/**
 * Object Chill provides Kryo serialization and deserialization functionality.
 * It contains a Kryo serialization pool and implicit methods for serialization and deserialization.
 *
 * The serializer method creates a new instance of Serializer[V].
 *
 * The serialize method converts a value of type V to a PlainText using Kryo serialization.
 *
 * The deserialize method converts a PlainText back to a value of type V using Kryo deserialization.
 */
object Chill {

  private val kryoPool =
    KryoPool.withByteArrayOutputStream(10, new ScalaKryoInstantiator())

  implicit def serializer[V]: Serializer[V] = new Serializer[V] {

    def serialize(value: V): PlainText = kryoPool.toBytesWithClass(value)

    def deserialize(plainText: PlainText): Try[V] = Try {
      kryoPool.fromBytes(plainText).asInstanceOf[V]
    }
  }
}
