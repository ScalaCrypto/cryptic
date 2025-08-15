package cryptic
package crypto

import Aes.{AesParams, AesPassphrase}

import java.security.{KeyPair, PrivateKey, PublicKey}
import javax.crypto.Cipher

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
object RsaAse:
  export java.security.{KeyPair, PrivateKey, PublicKey}
  export javax.crypto.spec.SecretKeySpec

  given encrypt(using key: PublicKey, aesParams: AesParams): Encrypt =
    (plainText: PlainText) =>
      given passphrase: AesPassphrase = AesPassphrase(30) //Todo parametrize
      val cipherText: CipherText = Aes.encrypt(plainText)
      val cipher: Cipher = Rsa.newCipher(Cipher.ENCRYPT_MODE, key)
      val encryptedPassphrase = cipher.doFinal(passphrase.bytes.mutable)
      CipherText(
        plainText.manifest,
        encryptedPassphrase.immutable,
        cipherText.bytes
      )

  given decrypt(using key: PrivateKey, aesParams: AesParams): Decrypt =
    (cipherText: CipherText) =>
      val IArray(manifest, keyBytes, textBytes) = cipherText.split
      val cipher: Cipher = Rsa.newCipher(Cipher.DECRYPT_MODE, key)
      val aesKey = cipher.doFinal(keyBytes.mutable).immutable
      val aesText = CipherText(textBytes)
      given passphrase: AesPassphrase = AesPassphrase(aesKey)
      Aes.decrypt(aesText).map(pt => PlainText(pt.bytes, manifest))

  /** Generates a new RSA key pair with the specified key size.
    *
    * @param size
    *   the size of the keys to generate, in bits
    * @return
    *   a new KeyPair instance containing the generated public and private keys
    */
  def keygen(size: Int): KeyPair = Rsa.keygen(size)
