package cryptic
package serialization

trait Serializer[V] {
  def serialize(value: V): PlainText
  def deserialize(plainText: PlainText): Either[String, V]
}
object Serializer {
  implicit val nothingSerializer: Serializer[Nothing] =
    new Serializer[Nothing] {
      override def serialize(value: Nothing): PlainText =
        throw new UnsupportedOperationException("serialize nothing")
      override def deserialize(plainText: PlainText): Either[String, Nothing] =
        throw new UnsupportedOperationException("deserialize nothing")
    }
}
