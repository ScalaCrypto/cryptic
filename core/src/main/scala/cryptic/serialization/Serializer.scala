package cryptic
package serialization

trait Serializer[V] {
  def serialize(value: V): PlainText
  def deserialize(plainText: PlainText): Either[String, V]
}
object Serializer {
  implicit val nothingSerializer: Serializer[Nothing] = new Serializer[Nothing] {
    override def serialize(value: Nothing) = throw new UnsupportedOperationException("serialize nothing")
    override def deserialize(plainText: PlainText) = throw new UnsupportedOperationException("deserialize nothing")
  }
  /*
  implicit def optionSerializer[V : Serializer]: Serializer[Option[V]] = new Serializer[_root_.scala.Option[V]] {
    override def serialize(value: Option[V]) = value match {
      case Some(v) => implicitly[Serializer[V]].serialize(v)
      case None => implicitly[Serializer[None.type]].serialize(None)
    }
    override def deserialize(plainText: PlainText) = implicitly[Serializer[V]].deserialize(plainText).map(Option.apply)
  }
  */
}
