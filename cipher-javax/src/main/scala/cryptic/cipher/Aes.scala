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

/** `Aes` is an implementation of the AES (Advanced Encryption Standard)
  * encryption algorithm with support for encryption and decryption in GCM mode
  * with no padding. It provides a secure method for symmetric encryption using
  * a passphrase-derived key.
  *
  * This object encapsulates the AES configuration and operations, including the
  * generation of initialization vectors, parameter specifications, and the
  * handling of encryption and decryption processes. It is built on top of
  * Java's Cryptography Architecture (JCA) and uses key derivation functionality
  * for secure passphrase handling.
  *
  * Configuration:
  *   - `transformation`: Specifies the cryptographic transformation, fixed as
  *     "GCM/NoPadding".
  *   - `saltLength`: Length of the salt used for key derivation.
  *   - `gcmTagLength`: Length of the GCM authentication tag in bits.
  *   - `ivLength`: Length of the initialization vector (IV) in bytes.
  *   - `factoryAlgorithm`: Algorithm used for password-based key derivation,
  *     fixed as "PBKDF2WithHmacSHA256".
  *   - `keyAlgorithm`: Key generation algorithm, AES in this case.
  *   - `iterationCount`: Iteration count for the key derivation process.
  *   - `keyLength`: Length of the encryption key in bits.
  *   - `version`: Represents the versioning of the cipher text for
  *     compatibility checks.
  *
  * This object defines the following:
  *   - `newCipher`: Creates and initializes a `Cipher` instance for encryption
  *     or decryption.
  *   - `newIv`: Generates a random initialization vector of the specified
  *     length.
  *   - `paramSpec`: Generates an `AlgorithmParameterSpec` for GCM mode with the
  *     provided IV.
  *
  * Implicit Behaviors:
  *   - `pbeKeyParams`: Defines the parameters for password-based key
  *     derivation.
  *   - `encrypt`: Implicitly provides the encryption logic, utilizing the
  *     passphrase-derived key. Encrypts the plaintext with the generated IV and
  *     constructs a `CipherText` object.
  *   - `decrypt`: Implicitly provides the decryption logic, utilizing the
  *     passphrase-derived key. Validates and decrypts the `CipherText` back to
  *     plaintext.
  *
  * Internal Utilities:
  *   - Encapsulation of salts, IVs, and key derivation parameters for secure
  *     handling of symmetric cryptographic operations.
  *   - Support for versioning to ensure compatibility between different cipher
  *     text formats.
  *
  * Use this object to encrypt and decrypt data securely, ensuring proper
  * handling of keys, salts, and cryptographic parameters.
  */
object Aes extends Symmetric[Try]:
  val transformation: String = "GCM/NoPadding"
  val saltLength: Int = 32
  val gcmTagLength = 128 // GCM authentication tag length in bits
  val ivLength: Int = 12
  val factoryAlgorithm: String = "PBKDF2WithHmacSHA256"
  val keyAlgorithm = "AES"
  val iterationCount: Int = 310000
  val keyLength: Int = 256
  val version: Version = FixedVersion(0, 0, 0, 1)

  object default:
    export cryptic.default.{given, *}
    export Aes.{given, *}

  /** Provides an implicit instance of `PBEKeyParams`, initializing it with
    * algorithm-specific parameters such as factory algorithm, iteration count,
    * key length, and key algorithm. These parameters are used for deriving
    * secret keys in password-based encryption (PBE) operations.
    *
    * @return
    *   an instance of `PBEKeyParams` containing pre-configured settings for
    *   password-based encryption.
    */
  given pbeKeyParams: PBEKeyParams =
    PBEKeyParams(factoryAlgorithm, iterationCount, keyLength, keyAlgorithm)

  override def newCipher(
      mode: Int,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  ): Cipher =
    val cipher = Cipher.getInstance(s"$keyAlgorithm/$transformation")
    cipher.init(mode, key, spec)
    cipher

  def newIv(): IArray[Byte] = secureRandom.newBytes(ivLength)

  def paramSpec(iv: IArray[Byte]): AlgorithmParameterSpec =
    new GCMParameterSpec(gcmTagLength, iv.mutable)

  given encrypt(using passphrase: Passphrase): Encrypt[Try] =
    (plainText: PlainText) =>
      Try:
        val salt = Salt(saltLength)
        val key = passphrase.secretKey(salt)
        val iv = newIv()
        val ivSpec = paramSpec(iv)
        encrypt(plainText, key, ivSpec).map(encrypted =>
          CipherText(
            version.bytes,
            plainText.aad.bytes,
            salt.bytes,
            iv,
            pbeKeyParams.bytes,
            encrypted
          )
        )
      .flatten

  given decrypt(using passphrase: Passphrase): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(ver, aadBytes, salt, iv, paramBytes, bytes)
          if version.supports(ver) =>
        val aad = AAD(aadBytes)
        val key =
          passphrase.secretKey(Salt(salt))(using PBEKeyParams(paramBytes))
        val ivSpec = paramSpec(iv)
        decrypt(bytes, aad, key, ivSpec).map(decrypted =>
          PlainText(decrypted, aad)
        )
      case IArray(ver, _, _, _, _, _) =>
        version.failed(ver)
