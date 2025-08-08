package cryptic
package crypto

import cryptic.codec.{Chill, Fst, Upickle}
import cryptic.crypto.Aes.AESParams
import cryptic.{Codec, Decrypt, Encrypt}
import upickle.default.{*, given}

trait AesSpecBase extends CryptoSpecBase:
  given aesPassword: Aes.AESPassphrase =
    Aes.AESPassphrase("correct horse battery staple")
  given aesParams: AESParams = AESParams()
  override given decrypt: Decrypt = Aes.decrypt
  override given encrypt: Encrypt = Aes.encrypt

class AesChillSpec extends AesSpecBase:
  override given stringCodec: Codec[String] = Chill.codec
class AesFstSpec extends AesSpecBase:
  override given stringCodec: Codec[String] = Fst.codec
class AesUpickleSpec[V: ReadWriter] extends AesSpecBase:
  override given stringCodec: Codec[String] = Upickle.codec
