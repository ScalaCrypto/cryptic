package cryptic
package crypto

import java.security.{PrivateKey, PublicKey}
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
  given encrypt(using publicKey: PublicKey): Encrypt[Try] =
    (plainText: PlainText) =>
      Try:
        val aesKey = Aes.keygen()
        val iv = Aes.newIv()
        val ivSpec = Aes.paramSpec(iv)
        for
          encryptedAesKey <- Rsa.encrypt(aesKey.getEncoded.immutable, publicKey)
          encryptedText <- Aes.encrypt(
            plainText.bytes,
            plainText.aad,
            aesKey,
            ivSpec
          )
        yield CipherText(
          plainText.aad,
          iv,
          encryptedAesKey,
          encryptedText
        )
      .flatten

  given decrypt(using privateKey: PrivateKey): Decrypt[Try] =
    (cipherText: CipherText) =>
      Try:
        val IArray(aad, iv, encryptedKey, encryptedText) = cipherText.split
        val ivSpec = Aes.paramSpec(iv)
        for
          aesKey <- Rsa.decrypt(encryptedKey, privateKey).map(Aes.key)
          text <- Aes.decrypt(encryptedText, aad, aesKey, ivSpec)
        yield PlainText(text, aad)
      .flatten
