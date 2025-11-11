package cryptic
package cipher

import java.security.{PrivateKey, PublicKey}
import scala.util.{Success, Try}

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
  val version = FixedVersion(0, 0, 0, 1)

  given encrypt(using publicKey: PublicKey): Encrypt[Try] =
    (plainText: PlainText) =>
      Try:
        val aesKey = Aes.keygen()
        val iv = Aes.newIv()
        val ivSpec = Aes.paramSpec(iv)
        val encryptedText = Aes.encrypt(plainText, aesKey, ivSpec)
        val aesKeyBytes = aesKey.getEncoded.immutable
        for
          encryptedText <- Aes.encrypt(plainText, aesKey, ivSpec)
          encryptedAesKey <- Rsa.encrypt(aesKeyBytes, publicKey)
        yield
          CipherText(
            version.bytes,
            plainText.aad,
            iv,
            encryptedAesKey,
            encryptedText
          )
      .flatten

  given decrypt(using privateKey: PrivateKey): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(ver, aad, iv, encryptedKey, encryptedText) if version.supports(ver) =>
        for
          aesKeyBytes <- Rsa.decrypt(encryptedKey, privateKey)
          aesKey = Aes.key(aesKeyBytes)
          ivSpec = Aes.paramSpec(iv)
          text <- Aes.decrypt(encryptedText, aad, aesKey, ivSpec)
        yield
          PlainText(text, aad)
      case IArray(ver, _,_,_,_) =>
        version.failed(ver)
