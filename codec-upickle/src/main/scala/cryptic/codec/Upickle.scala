package cryptic
package codec

import upickle.default.*

import scala.util.Try

/** The Upickle object provides functionalities for encoding and
  * decoding data.
  *
  * The apply method needs a given ReadWriter instance for the specified type
  * and returns a Codec instance for the same type. The Codec instance
  * can convert objects of that type to and from plain text format using the
  * uPickle library methods.
  */
object Upickle:
  def apply[V: ReadWriter](): Codec[V] =
    new Codec[V]:
      override def encode(value: V): PlainText = PlainText(write(value))
      override def decode(plainText: PlainText): Try[V] = Try(
        read[V](plainText.bytes)
      )
