package cryptic
package serialization

import org.nustaq.serialization.FSTConfiguration

object FstSerializer {
  val fst = FSTConfiguration.createDefaultConfiguration()
  implicit def serializer[V]: Serializer[V] = new Serializer[V] {
    override def serialize(value: V) = PlainText(fst.asByteArray(value))
    override def deserialize(plainText: PlainText) = Right(fst.asObject(plainText).asInstanceOf[V])
  }
  /*
  implicit val serializer: Serializer[Any] = (value: Any) => PlainText(fst.asByteArray(value))
  implicit val deserializer: Deserializer[Any] = (plainText: PlainText) => Right(fst.asObject(plainText))
  */
}