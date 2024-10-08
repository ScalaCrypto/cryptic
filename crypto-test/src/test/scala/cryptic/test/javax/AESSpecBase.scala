package cryptic
package test
package javax

import cryptic.{Decrypt, Encrypt}
import cryptic.crypto.AES
import cryptic.crypto.AES.AESParams
import cryptic.serialization.{Chill, Fst, Serializer, Upickle}
import cryptic.test.CryptoSpecBase
import upickle.default

abstract class AESSpecBase extends CryptoSpecBase {
  implicit val aesPassword: AES.AESPassphrase =
    AES.AESPassphrase("correct horse battery staple")
  implicit val aesParams: AESParams = AESParams()
  override val decrypt: Decrypt = AES.decrypt
  val encrypt: Encrypt = AES.encrypt
}

class AESChillSpec extends AESSpecBase {
  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Chill.serializer
}
class AESFstSpec extends AESSpecBase {
  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Fst.serializer
}
class AESUpickleSpec extends AESSpecBase {
  override implicit def serializer[V](implicit rw: default.ReadWriter[V]): Serializer[V] = Upickle[V]
}
