package cryptic
package crypto

import java.security.{PrivateKey, PublicKey}
import scala.concurrent.ExecutionContext
import scala.util.Try

/** RsaAse object provides encryption, decryption, and key generation
  * functionalities using the RSA algorithm to create an AES key that is
  * encrypted using RSA and stored together with the encrypted PlainText.
  *
  * @define encrypt
  *   Performs encryption on the given plain text using the provided public key.
  * @define decrypt
  *   Performs decryption on the given cipher text using the provided private
  *   key.
  * @define keygen
  *   Generates an RSA key pair with the specified size.
  */
object RsaAes:
  given encrypt(using
      publicKey: PublicKey,
      ec: ExecutionContext
  ): Encrypt =
    (plainText: PlainText) =>
      val aesKey = Aes.keygen()
      val iv = Aes.newIv()
      val ivSpec = Aes.paramSpec(iv)
      for
        encryptedText <- Aes.encrypt(plainText.bytes, aesKey, ivSpec)
        encryptedKey <- Rsa.encrypt(aesKey.getEncoded.immutable, publicKey)
      yield CipherText(
        plainText.manifest,
        iv.immutable,
        encryptedKey,
        encryptedText
      )

  given decrypt(using
      privateKey: PrivateKey,
      ec: ExecutionContext
  ): Decrypt =
    (cipherText: CipherText) =>
      val IArray(manifest, iv, keyBytes, textBytes) = cipherText.split
      for
        aesKeyBytes <- Rsa.decrypt(keyBytes, privateKey)
        aesKey = Aes.key(aesKeyBytes)
        ivSpec = Aes.paramSpec(iv.mutable)
        text <- Aes.decrypt(textBytes, aesKey, ivSpec)
      yield PlainText(text, manifest)
