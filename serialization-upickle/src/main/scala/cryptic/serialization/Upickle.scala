package cryptic
package serialization

import upickle.default.*

import scala.util.Try

/** The Upickle object provides functionalities for serializing and
  * deserializing data.
  *
  * The apply method needs a given ReadWriter instance for the specified type
  * and returns a Serializer instance for the same type. The Serializer instance
  * can convert objects of that type to and from plain text format using the
  * uPickle library methods.
  */
object Upickle:
  def apply[V: ReadWriter](): Serializer[V] =
    new Serializer[V]:
      override def serialize(value: V): PlainText = PlainText(write(value))
      override def deserialize(plainText: PlainText): Try[V] = Try(
        read[V](plainText)
      )
