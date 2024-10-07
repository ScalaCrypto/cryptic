package cryptic
package serialization

import org.nustaq.serialization.FSTConfiguration

object Fst {
  val fst: FSTConfiguration = FSTConfiguration.createDefaultConfiguration()
  implicit def serializer[V]: Serializer[V] = new Serializer[V] {
    override def serialize(value: V): PlainText = PlainText(fst.asByteArray(value))
    override def deserialize(plainText: PlainText): Right[String, V] = Right(fst.asObject(plainText).asInstanceOf[V])
  }
}
