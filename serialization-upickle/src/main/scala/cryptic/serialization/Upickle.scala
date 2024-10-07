package cryptic
package serialization

import upickle.default._

import scala.util.Try

object Upickle {
  def apply[V](implicit readWriter: ReadWriter[V]): Serializer[V] =
    new Serializer[V] {

      override def serialize(value: V): PlainText = PlainText(write(value)(readWriter))

      override def deserialize(plainText: PlainText): Either[String, V] =
        Try(read[V](plainText)(readWriter)).fold(t => Left(t.getMessage), Right.apply)
    }
}
