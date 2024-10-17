package cryptic
package serialization

import org.nustaq.serialization.FSTConfiguration

import scala.util.Try

/**
 * The `Fst` object provides a mechanism for serialization and deserialization using the FST (Fast Serialization) library.
 *
 * It contains a default FST configuration and an implicit `Serializer` for generic types.
 *
 * Members:
 * - `fst`: A default FST configuration instance created using `FSTConfiguration.createDefaultConfiguration()`.
 *
 * Methods:
 * - `serializer[V]`: An implicit method that provides a `Serializer` implementation for type `V`. This implementation
 * includes `serialize` and `deserialize` methods.
 *   - The `serialize` method converts a value of type `V` into `PlainText` by serializing it to a byte array using the
 *     FST configuration instance.
 *   - The `deserialize` method attempts to convert a `PlainText` instance back into a value of type `V` by deserializing
 *     the byte array using the FST configuration instance, wrapped in a `Try`.
 */
object Fst {
  val fst: FSTConfiguration = FSTConfiguration.createDefaultConfiguration()
  implicit def serializer[V]: Serializer[V] = new Serializer[V] {
    override def serialize(value: V): PlainText = PlainText(fst.asByteArray(value))
    override def deserialize(plainText: PlainText): Try[V] = Try(fst.asObject(plainText).asInstanceOf[V])
  }
}
