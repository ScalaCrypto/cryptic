package cryptic
package cipher

import java.security.{PrivateKey, PublicKey}
import scala.util.{Success, Try}

/** Provides RSA-AES hybrid encryption and decryption functionality.
  *
  * This object implements a cryptographic system combining RSA and AES
  * algorithms to achieve both strong encryption and efficient performance. RSA
  * is used to encrypt AES keys, while AES is employed to encrypt plaintext
  * data.
  *
  * The implementation also includes support for specifying cryptographic keys,
  * initialization vectors, and advanced encryption configurations through a set
  * of given instances.
  *
  * Components:
  *   - `version`: A fixed version identifier for the cipher configuration.
  *   - `keyAlgorithm`: The algorithm name for AES key generation.
  *   - `Functor[Try]`: An implicit functor instance for the `Try` effect type,
  *     providing functional combinators for handling computations that may
  *     fail.
  *   - `default`: A sub-object providing default exports for common
  *     cryptographic utilities, as well as RSA key pair generation and implicit
  *     instances related to `RsaAes`.
  *
  * Given Instances:
  *   - `encrypt`: A given instance implementing the `Encrypt[Try]` type class,
  *     requiring a `PublicKey`. This instance provides a method to encrypt
  *     plaintext using AES with a generated key, the key itself being encrypted
  *     using RSA.
  *   - `decrypt`: A given instance implementing the `Decrypt[Try]` type class,
  *     requiring a `PrivateKey`. This instance provides a method to decrypt
  *     cipher text by first decrypting the AES key using RSA and then
  *     decrypting the data using AES.
  */
object RsaAes:

  val version = FixedVersion(0, 0, 0, 1)
  val keyAlgorithm = "AES"
  given Functor[Try] = Functor.tryFunctor

  object default:
    export cryptic.default.{given, *}
    export RsaAes.{given, *}
    export Rsa.newKeyPair

  given encrypt(using publicKey: PublicKey): Encrypt[Try] =
    (plainText: PlainText) =>
      Try:
        val aesKey = Aes.generateSecretKey()
        val iv = Aes.newIv()
        val ivSpec = Aes.paramSpec(iv)
        val encryptedText = Aes.encrypt(plainText, aesKey, ivSpec)
        val aesKeyBytes = aesKey.getEncoded.immutable
        for
          encryptedText <- Aes.encrypt(plainText, aesKey, ivSpec)
          encryptedAesKey <- Rsa.encrypt(aesKeyBytes, publicKey)
        yield CipherText(
          version.bytes,
          plainText.aad.bytes,
          iv,
          encryptedAesKey,
          encryptedText
        )
      .flatten

  given decrypt(using privateKey: PrivateKey): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(ver, aadBytes, iv, encryptedKey, encryptedText)
          if version.supports(ver) =>
        val aad = AAD(aadBytes)
        for
          aesKeyBytes <- Rsa.decrypt(encryptedKey, privateKey)
          aesKey = Aes.secretKey(aesKeyBytes)
          ivSpec = Aes.paramSpec(iv)
          text <- Aes.decrypt(encryptedText, aad, aesKey, ivSpec)
        yield PlainText(text, aad)
      case IArray(ver, _, _, _, _) =>
        version.failed(ver)
