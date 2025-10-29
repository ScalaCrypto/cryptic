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
  given version: Versioning = FixedVersion(0, 0, 0, 1)

  given encrypt(using publicKey: PublicKey): Encrypt[Try] =
    (plainText: PlainText) =>
      Try:
        val aesKey = Aes.keygen()
        val iv = Aes.newIv()
        val ivSpec = Aes.paramSpec(iv)
        val encryptedText = Aes.encrypt(plainText, aesKey, ivSpec)
        val aesKeyBytes = aesKey.getEncoded.immutable
        val encryptedAesKey = Rsa.encrypt(aesKeyBytes, publicKey)
        CipherText(
          version.bytes,
          plainText.aad,
          iv,
          encryptedAesKey,
          encryptedText
        )

  given decrypt(using privateKey: PrivateKey): Decrypt[Try] =
    (_: CipherText).withVersion:
      case IArray(_, aad, iv, encryptedKey, encryptedText) =>
        val aesKeyBytes = Rsa.decrypt(encryptedKey, privateKey)
        val aesKey = Aes.key(aesKeyBytes)
        val ivSpec = Aes.paramSpec(iv)
        val text = Aes.decrypt(encryptedText, aad, aesKey, ivSpec)
        PlainText(text, aad)
