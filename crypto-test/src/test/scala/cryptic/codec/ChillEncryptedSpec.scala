package cryptic
package codec

import upickle.default.*

class ChillEncryptedSpec extends EncryptedSpecBase:
  override def codec[V](implicit rw: ReadWriter[V]): Codec[V] = Chill.codec
