package cryptic
package cipher

import cryptic.codec.{Chill, Fst, Upickle}
import cryptic.cipher.Aes.{*, given}
import cryptic.{Codec, Decrypt, Encrypt}
import upickle.default.{*, given}

import scala.util.Try

trait AesSpecBase extends CipherSpecBase:
  given aesPassword: Passphrase =
    Passphrase("correct horse battery staple")
  override given decrypt: Decrypt[Try] = Aes.decrypt
  override given encrypt: Encrypt[Try] = Aes.encrypt

class AesChillSpec extends AesSpecBase:
  override given stringCodec: Codec[String] = Chill.codec
class AesFstSpec extends AesSpecBase:
  override given stringCodec: Codec[String] = Fst.codec
class AesUpickleSpec[V: ReadWriter] extends AesSpecBase:
  override given stringCodec: Codec[String] = Upickle.codec
