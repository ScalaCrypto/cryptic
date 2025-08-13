package cryptic
package codec

import org.nustaq.serialization.FSTConfiguration

import scala.util.Try

/** The `Fst` object provides a mechanism for encoding and decoding using the
  * FST (Fast Serialization) library.
  *
  * It contains a default FST configuration and a given `Codec` for generic
  * types.
  *
  * Members:
  *   - `fst`: A default FST configuration instance created using
  *     `FSTConfiguration.createDefaultConfiguration()`.
  *
  * Methods:
  *   - `codec[V]`: A given that provides a `Codec` implementation for type `V`.
  *     This implementation includes `encode` and `decode` methods.
  *     - The `encode` method converts a value of type `V` into `PlainText` by
  *       encoding it to a byte array using the FST configuration instance.
  *     - The `decode` method attempts to convert a `PlainText` instance back
  *       into a value of type `V` by decoding the byte array using the FST
  *       configuration instance, wrapped in a `Try`.
  */
object Fst extends Codec.Companion:
  private val fst: FSTConfiguration =
    FSTConfiguration.createDefaultConfiguration()
  given codec: [V] => Codec[V]:
    def encode(v: V, manifest: Manifest): PlainText =
      PlainText(fst.asByteArray(v).immutable, manifest)
    def decode(pt: PlainText): Try[V] = Try(
      fst.asObject(pt.bytes.mutable).asInstanceOf[V]
    )
