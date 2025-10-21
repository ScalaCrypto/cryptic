package cryptic
package codec

import org.nustaq.serialization.FSTConfiguration

import scala.util.Try

/** FST (Fast Serialization) based Codec for arbitrary values.
  *
  * Uses a shared `FSTConfiguration` to encode values to bytes and decode them back.
  *
  * Notes:
  * - Encoding stores FST-produced bytes into `PlainText.bytes` and passes the given
  *   `AAD` through without interpretation.
  * - Decoding reads from `PlainText.bytes`; the AAD is ignored by this codec.
  * - Binary wire format; not human-readable. Ensure compatible classpaths and FST
  *   configuration on both ends.
  */
object Fst extends Codec.Companion:
  private val fst: FSTConfiguration =
    FSTConfiguration.createDefaultConfiguration()
  given codec: [V] => Codec[V]:
    def encode(v: V, aad: AAD): PlainText =
      PlainText(fst.asByteArray(v).immutable, aad)
    def decode(pt: PlainText): Try[V] = Try(
      fst.asObject(pt.bytes.mutable).asInstanceOf[V]
    )
