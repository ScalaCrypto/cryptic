package cryptic
package serialization

import upickle.default._

import scala.util.Try

/**
 * The Upickle object provides functionalities for serializing and deserializing data.
 *
 * The apply method takes an implicit ReadWriter instance and returns a Serializer instance.
 * The Serializer instance can convert data to and from plain text format using uPickle library methods.
 */
object Upickle {
  def apply[V](implicit readWriter: ReadWriter[V]): Serializer[V] =
    new Serializer[V] {

      override def serialize(value: V): PlainText = PlainText(write(value)(readWriter))

      override def deserialize(plainText: PlainText): Try[V] = Try(read[V](plainText)(readWriter))
    }
}
