package cryptic
package codec

import upickle.default.*

import scala.util.Try

/** uPickle-based Codec for JSON-ish serialization of arbitrary values.
  *
  * Requires uPickle `Writer` and `Reader` type class instances to be in scope for `V`.
  *
  * Notes:
  * - Encoding uses `upickle.default.write` to produce UTF-8 bytes and preserves the
  *   supplied `AAD` unchanged.
  * - Decoding uses `upickle.default.read` from `PlainText.bytes`; the AAD is ignored
  *   by this codec.
  * - Unlike the binary codecs, this produces a human-readable format but still treats
  *   the payload as bytes inside `PlainText`.
  */
object Upickle extends Codec.Companion:
  given codec: [V: {Writer, Reader}] => Codec[V]:
    def encode(v: V, aad: AAD): PlainText = PlainText(write(v), aad)
    def decode(pt: PlainText): Try[V] = Try(
      read[V](pt.bytes.mutable)
    )
