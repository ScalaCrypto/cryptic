package cryptic
package cipher

import java.security.SecureRandom
import java.security.spec.AlgorithmParameterSpec
import javax.crypto.spec.{
  GCMParameterSpec,
  IvParameterSpec,
  PBEKeySpec,
  SecretKeySpec
}
import javax.crypto.{Cipher, KeyGenerator, SecretKey, SecretKeyFactory}
import scala.util.{Success, Try}

/** Provides AES encryption and decryption functionality using GCM mode with
 * no padding.
 *
 * This object is a concrete implementation of the `Symmetric[Try]` trait,
 * enabling encryption and decryption processes for securing data. The
 * object leverages configuration properties such as transformation mode,
 * cryptographic salt, initialization vector (IV) length, and factory
 * algorithm for key generation.
 *
 * Key features:
 * - Supports AES encryption in GCM mode with no padding for authenticated
 * encryption.
 * - Manages cryptographic parameters such as salt, IV, and versioning.
 * - Generates new cryptographic keys and IVs securely.
 *
 * Components:
 * - `encrypt`: A given instance of `Encrypt[Try]` for encrypting plaintext.
 * - `decrypt`: A given instance of `Decrypt[Try]` for decrypting ciphertext.
 * - `newIv`: Generates a new initialization vector with the specified
 * length.
 * - `paramSpec`: Constructs a GCM parameter specification for the given IV.
 * - `newCipher`: Creates and initializes a new cryptographic cipher.
 *
 * The object includes configuration constants:
 * - `transformation`: The cryptographic transformation mode.
 * - `saltLength`: Length of the salt in bytes.
 * - `gcmTagLength`: Length of the GCM authentication tag in bits.
 * - `ivLength`: Length of the initialization vector in bytes.
 * - `factoryAlgorithm`: Algorithm used for key generation.
 * - `keyAlgorithm`: Algorithm used for encryption.
 * - `keyspecIterationCount`: Number of iterations for the key derivation
 * function.
 * - `keyspecLength`: Length of the derived key in bits.
 * - `version`: The version information for the cipher text format.
 *
 * The `default` object imports implicit values and utilities for using this
 * implementation in a broader cryptographic framework.
 */
object Aes extends Symmetric[Try]:
  val transformation: String = "GCM/NoPadding"
  val saltLength: Int = 32
  val gcmTagLength = 128 // GCM authentication tag length in bits
  val ivLength: Int = 12
  val factoryAlgorithm: String = "PBKDF2WithHmacSHA256"
  val keyAlgorithm = "AES"
  val keyspecIterationCount: Int = 310000
  val keyspecLength: Int = 256
  val version: Version = FixedVersion(0, 0, 0, 1)

  object default:
    export cryptic.default.{given ,*}
    export Aes.{given, *}

  override def newCipher(
      mode: Int,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  ): Cipher =
    val cipher = Cipher.getInstance(s"  $keyAlgorithm/$transformation")
    cipher.init(mode, key, spec)
    cipher

  def newIv(): IArray[Byte] = secureRandom.newBytes(ivLength)

  def paramSpec(iv: IArray[Byte]): AlgorithmParameterSpec =
    new GCMParameterSpec(gcmTagLength, iv.mutable)

  given encrypt(using passphrase: Passphrase): Encrypt[Try] =
    (plainText: PlainText) =>
      Try:
        val salt = Salt(saltLength)
        val key = keygen(
          passphrase,
          salt,
          factoryAlgorithm,
          keyspecIterationCount,
          keyspecLength,
          keyAlgorithm
        )
        val iv = newIv()
        val ivSpec = paramSpec(iv)
        encrypt(plainText, key, ivSpec).map(encrypted =>
          CipherText(
            version.bytes,
            plainText.aad,
            salt.bytes,
            iv,
            encrypted
          )
        )
      .flatten

  given decrypt(using passphrase: Passphrase): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(ver, aad, salt, iv, bytes) if version.supports(ver) =>
        val key = keygen(
          passphrase,
          Salt(salt),
          factoryAlgorithm,
          keyspecIterationCount,
          keyspecLength,
          keyAlgorithm
        )
        val ivSpec = paramSpec(iv)
        decrypt(bytes, aad, key, ivSpec).map(decrypted =>
          PlainText(decrypted, aad)
        )
      case IArray(ver, _, _, _, _) =>
        version.failed(ver)
