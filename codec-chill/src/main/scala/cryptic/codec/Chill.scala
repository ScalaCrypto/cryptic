package cryptic
package codec

import com.twitter.chill.{KryoPool, ScalaKryoInstantiator}

import scala.util.Try

/** Kryo-based Codec instances for arbitrary Scala values.
  *
  * Provides a small Kryo pool and a generic `given Codec[V]` that serializes values
  * using Twitter Chill/Kryo.
  *
  * Notes:
  * - Encoding writes the Kryo bytes directly into `PlainText.bytes` and preserves the
  *   provided `Manifest` unchanged; the manifest is not interpreted by the codec.
  * - Decoding reads the `PlainText.bytes` back using the same Kryo configuration and
  *   ignores the manifest.
  * - This codec is binary and not human-readable. It requires consistent classpath
  *   across encryption/decryption boundaries.
  */
object Chill extends Codec.Companion:
  private val kryoPool =
    KryoPool.withByteArrayOutputStream(10, new ScalaKryoInstantiator())

  given codec: [V] => Codec[V]:
    def encode(v: V, aad: AAD): PlainText = PlainText(
      kryoPool.toBytesWithClass(v).immutable, aad
    )
    def decode(pt: PlainText): Try[V] = Try(
      kryoPool.fromBytes(pt.bytes.mutable).asInstanceOf[V]
    )
