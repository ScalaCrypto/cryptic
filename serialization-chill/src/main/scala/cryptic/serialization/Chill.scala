package cryptic
package serialization

import com.twitter.chill.{KryoPool, ScalaKryoInstantiator}

import scala.util.Try

object Chill {

  private val kryoPool =
    KryoPool.withByteArrayOutputStream(10, new ScalaKryoInstantiator())

  implicit def serializer[V]: Serializer[V] = new Serializer[V] {

    def serialize(value: V): PlainText = kryoPool.toBytesWithClass(value)

    def deserialize(plainText: PlainText): Either[String, V] = Try {
      kryoPool.fromBytes(plainText).asInstanceOf[V]
    }.fold(t => Left(t.getMessage), Right.apply)
  }
}
