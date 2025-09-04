package cryptic
package crypto

import java.security.*
import javax.crypto.Cipher
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/** Trait representing asymmetric encryption and decryption functionality.
  *
  * Provides a mechanism to encrypt and decrypt data using public and private
  * keys following asymmetric cryptographic principles.
  */
trait Asymmetric:
  def newCipher(mode: Int, key: Key): Cipher

  given encrypt(using key: PublicKey, ec: ExecutionContext): Encrypt =
    (plainText: PlainText) =>
      encrypt(plainText.bytes, key).map(encrypted =>
        CipherText(plainText.manifest, encrypted)
      )

  def encrypt(bytes: IArray[Byte], key: PublicKey): Future[IArray[Byte]] =
    val cipher: Cipher = newCipher(Cipher.ENCRYPT_MODE, key)
    Future.successful(cipher.doFinal(bytes.mutable).immutable)

  given decrypt(using key: PrivateKey, ec: ExecutionContext): Decrypt =
    (cipherText: CipherText) =>
      val IArray(manifest, bytes) = cipherText.split
      decrypt(bytes, key).map(text => PlainText(text, manifest))

  def decrypt(
      bytes: IArray[Byte],
      privateKey: PrivateKey
  ): Future[IArray[Byte]] =
    val cipher: Cipher = newCipher(Cipher.DECRYPT_MODE, privateKey)
    Future.successful(cipher.doFinal(bytes.mutable).immutable)
