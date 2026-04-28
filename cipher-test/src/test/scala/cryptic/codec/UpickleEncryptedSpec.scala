package cryptic
package codec

import upickle.default.*

class UpickleEncryptedSpec extends EncryptedSpecBase:
//  given rw:ReadWriter[Foo] = deferred

  override def codec[V: ReadWriter]: Codec[V] = Upickle.codec
