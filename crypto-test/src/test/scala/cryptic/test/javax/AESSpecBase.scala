package cryptic
package test
package javax

import cryptic.{Decrypt, Encrypt}
import cryptic.crypto.AES
import cryptic.crypto.AES.AESParams
import cryptic.codec.{Chill, Fst, Upickle}
import cryptic.test.CryptoSpecBase
import upickle.default.*

trait AESSpecBase extends CryptoSpecBase:
  given aesPassword: AES.AESPassphrase =
    AES.AESPassphrase("correct horse battery staple")
  given aesParams: AESParams = AESParams()
  override given decrypt: Decrypt = AES.decrypt
  override given encrypt: Encrypt = AES.encrypt

class AESChillSpec extends AESSpecBase:
  override given codec[V]: Codec[V] = Chill.codec
class AESFstSpec extends AESSpecBase:
  override given codec[V]: Codec[V] = Fst.codec
class AESUpickleSpec[V: ReadWriter] extends AESSpecBase:
  override given codec[W]: Codec[W] =
    Upickle[W]()(using summon[ReadWriter[V]].asInstanceOf[ReadWriter[W]])
