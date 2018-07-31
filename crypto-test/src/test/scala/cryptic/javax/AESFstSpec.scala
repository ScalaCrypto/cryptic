package cryptic.javax
import cryptic.crypto.AES
import cryptic.crypto.AES.AESParams
import cryptic.{Decrypt, Encrypt, FstSpecBase}
class AESFstSpec extends FstSpecBase {
  implicit val aesPassword: AES.AESPassphrase = AES.AESPassphrase("correct horse battery staple")
  implicit val aesParams: AESParams = AESParams()
  val encrypt: Encrypt = AES.encrypt
  override val decrypt: Decrypt = AES.decrypt
}
