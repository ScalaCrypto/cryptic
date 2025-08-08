package cryptic
package codec

import upickle.default.*

class FstEncryptedSpec extends EncryptedSpecBase:
  override def codec[V: ReadWriter]: Codec[V] = Fst.codec
