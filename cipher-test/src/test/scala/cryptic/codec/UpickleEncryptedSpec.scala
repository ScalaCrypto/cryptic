package cryptic
package codec

import upickle.default.*

class UpickleEncryptedSpec extends EncryptedSpecBase:
  override def codec[V: ReadWriter]: Codec[V] = Upickle.codec
