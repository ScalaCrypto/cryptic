package cryptic
package crypto

/** Simple reverse crypto...
  */
object Reverse {
  implicit val encrypt: Encrypt = (plainText: PlainText) ⇒
    CipherText(plainText.reverse)
  implicit val decrypt: Decrypt = (cipherText: CipherText) ⇒
    Right(PlainText(cipherText.bytes.reverse))
}
