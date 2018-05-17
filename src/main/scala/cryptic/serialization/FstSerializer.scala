package cryptic
package serialization

import org.nustaq.serialization.FSTConfiguration

object FstSerializer {
  val fst = FSTConfiguration.createDefaultConfiguration()
  implicit def serializer[V]: Serializer[V] = new Serializer[V] {
    override def serialize(value: V) = PlainText(fst.asByteArray(value))
    override def deserialize(plainText: PlainText) = Right(fst.asObject(plainText).asInstanceOf[V])
  }
}