package cryptic
package crypto

import cryptic.crypto.Aes.AesParams

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

  given encrypt(using
      publicKey: PublicKey,
      aesParams: AesParams
  ): Encrypt =
    (plainText: PlainText) =>
      val aesKey = Aes.keygen()
      val iv = aesParams.newIv()
      val ivSpec = aesParams.paramSpec(iv)
      val encryptedText =
        Aes.encrypt(plainText.bytes, aesKey, ivSpec, aesParams.mode)
      val encryptedAesKey =
        Rsa.encrypt(aesKey.getEncoded.immutable, publicKey)
      CipherText(
        plainText.manifest,
        iv.immutable,
        encryptedAesKey,
        encryptedText
      )

  given decrypt(using
      privateKey: PrivateKey,
      aesParams: AesParams
  ): Decrypt =
    (cipherText: CipherText) =>
      Try:
        val IArray(manifest, iv, keyBytes, textBytes) = cipherText.split
        val aesKeyBytes = Rsa.decrypt(keyBytes, privateKey)
        val aesKey = Aes.key(aesKeyBytes)
        val ivSpec = aesParams.paramSpec(iv.mutable)
        val text = Aes.decrypt(textBytes, aesKey, ivSpec, aesParams.mode)
        PlainText(text, manifest)
