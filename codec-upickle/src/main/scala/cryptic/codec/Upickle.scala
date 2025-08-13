package cryptic
package codec

import upickle.default.*

import scala.util.Try

/** The Upickle object provides functionalities for encoding and decoding data.
  *
  * The apply method needs a given ReadWriter instance for the specified type
  * and returns a Codec instance for the same type. The Codec instance can
  * convert objects of that type to and from plain text format using the uPickle
  * library methods.
  */
object Upickle extends Codec.Companion:
  given codec: [V: {Writer, Reader}] => Codec[V]:
    def encode(v: V, manifest: Manifest): PlainText = PlainText(write(v), manifest)
    def decode(pt: PlainText): Try[V] = Try(
      read[V](pt.bytes.mutable)
    )
