package cryptic
package serialization

import scala.util.{ Failure, Try }

trait Serializer[V] {
  def serialize(value: V): PlainText
  def deserialize(plainText: PlainText): Try[V]
}
object Serializer {
  implicit val nothingSerializer: Serializer[Nothing] =
    new Serializer[Nothing] {
      override def serialize(value: Nothing): PlainText =
        throw new UnsupportedOperationException("serialize nothing")
      override def deserialize(plainText: PlainText): Try[Nothing] =
        Failure(new UnsupportedOperationException("deserialize nothing"))
    }
}
