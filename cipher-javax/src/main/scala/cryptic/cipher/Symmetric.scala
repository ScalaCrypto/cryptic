package cryptic
package cipher

import java.security.SecureRandom
import java.security.spec.AlgorithmParameterSpec
import javax.crypto.{Cipher, KeyGenerator, SecretKey, SecretKeyFactory}
import javax.crypto.spec.{
  GCMParameterSpec,
  IvParameterSpec,
  PBEKeySpec,
  SecretKeySpec
}
import scala.util.Try

/** A trait that centralizes cryptographic operations relevant to symmetric
  * encryption and decryption, key generation, and key management.
  *
  * @tparam F
  *   A higher-kinded type representing the effect structure for computations
  *   involving cryptographic operations.
  */
trait Symmetric[F[_]]:
  /**
   * The cryptographic algorithm used for key generation and cipher operations.
   *
   * This value specifies the key algorithm utilized by the `Symmetric` class for
   * operations such as encryption, decryption, and key generation. It is used to
   * ensure compatibility and consistency with the cryptographic transformations
   * performed by the associated cipher instances.
   */
  val keyAlgorithm: String

  import cryptic.default.{given, *}

  /**
   * Creates and initializes a new cryptographic cipher for the specified mode,
   * secret key, and algorithm parameter specification.
   *
   * This method uses the configured cryptographic transformation mode and key algorithm
   * to create a cipher instance and initializes it with the provided mode, key,
   * and parameter specification. The created cipher can be used for encryption
   * or decryption based on the specified mode.
   *
   * @param mode
   * The operation mode for the cipher (e.g., Cipher.ENCRYPT_MODE or Cipher.DECRYPT_MODE).
   * @param key
   * The secret key used for initializing the cipher.
   * @param spec
   * The algorithm parameter specification used for configuring the cipher, such as
   * an initialization vector (IV) or GCM parameter specifications.
   * @return
   * A new instance of the initialized cipher configured with the specified parameters.
   */
  def newCipher(
      mode: Int,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  ): Cipher

  /**
   * Encrypts the provided plaintext bytes using a symmetric encryption algorithm
   * with the specified secret key and algorithm parameter specification.
   * Additional authenticated data (AAD) is incorporated into the encryption
   * process for integrity purposes.
   *
   * @param bytes
   * The plaintext bytes to be encrypted, represented as an immutable array.
   * @param aad
   * The additional authenticated data (AAD) to be associated with the encryption
   * process, used for ensuring data integrity. It is processed only when non-empty.
   * @param key
   * The secret key used for encryption.
   * @param spec
   * The algorithm parameter specification used to configure the cipher, such as
   * initialization vector (IV) or GCM parameter specifications.
   * @return
   * An effectful computation resulting in the encrypted data as an immutable array
   * of bytes.
   */
  def encrypt(
      bytes: IArray[Byte],
      aad: AAD,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  )(using Functor[F]): F[IArray[Byte]] =
    Try:
      val cipher = newCipher(Cipher.ENCRYPT_MODE, key, spec)
      aad.forNonEmpty(bytes => cipher.updateAAD(bytes.mutable))
      cipher.doFinal(bytes.mutable).immutable
    .lift

  /** Encrypts the given plaintext using the specified secret key and algorithm
   * parameter specification (e.g., initialization vector or GCM parameter spec).
   *
   * This method performs a symmetric encryption process, based on the provided
   * key and configuration, to secure the plaintext bytes. Additional
   * authenticated data (AAD) associated with the plaintext is included in the
   * encryption process for ensuring data integrity.
   *
   * @param plainText
   * The plaintext to be encrypted, containing the unencrypted payload bytes
   * and possible associated authentication data (AAD).
   * @param key
   * The secret key used for encryption.
   * @param spec
   * The cryptographic algorithm parameter specification used to initialize
   * the cipher, such as GCM or IV-based specifications.
   * @return
   * An effectful computation resulting in the encrypted bytes as an immutable
   * array.
   */
  def encrypt(
      plainText: PlainText,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  )(using Functor[F]): F[IArray[Byte]] =
    encrypt(plainText.bytes, plainText.aad, key, spec)

  /**
   * Decrypts the provided ciphertext bytes using a symmetric encryption algorithm
   * with the specified secret key and algorithm parameter specification.
   * Additional authenticated data (AAD) is incorporated into the decryption
   * process for integrity verification.
   *
   * @param bytes
   * The ciphertext bytes to be decrypted, represented as an immutable array.
   * @param aad
   * The additional authenticated data (AAD) associated with the encrypted data,
   * used for ensuring data integrity during decryption. It is processed only if non-empty.
   * @param key
   * The secret key used for decryption.
   * @param spec
   * The algorithm parameter specification used to configure the cipher, such as
   * initialization vector (IV) or GCM parameter specifications.
   * @return
   * An effectful computation resulting in the decrypted data as an immutable array
   * of bytes.
   */
  def decrypt(
      bytes: IArray[Byte],
      aad: AAD,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  )(using Functor[F]): F[IArray[Byte]] =
    Try:
      val cipher = newCipher(Cipher.DECRYPT_MODE, key, spec)
      aad.forNonEmpty(bytes => cipher.updateAAD(bytes.mutable))
      cipher.doFinal(bytes.mutable).immutable
    .lift

  /**
   * Generates a secret key using the specified key size and the configured key algorithm.
   *
   * This method creates and initializes a new key generator for the provided key size
   * and produces a secret key compatible with the given cryptographic algorithm.
   *
   * @param size
   * The size, in bits, of the secret key to be generated. Defaults to 256 bits if not specified.
   * @return
   * A new instance of SecretKey generated with the specified size and algorithm.
   */
  def generateSecretKey(size: Int = 256): SecretKey =
    val keyGenerator = KeyGenerator.getInstance(keyAlgorithm)
    keyGenerator.init(size)
    keyGenerator.generateKey()

  /**
   * Generates a secret key using the specified byte array and the configured key algorithm.
   *
   * This method creates a new secret key instance based on the provided immutable array of bytes.
   *
   * @param bytes
   * The immutable array of bytes that represents the secret key material.
   * The content of this array is utilized to construct the secret key.
   * @return
   * An instance of `SecretKey` created using the specified byte array and the configured key algorithm.
   */
  def secretKey(bytes: IArray[Byte]): SecretKey =
    new SecretKeySpec(bytes.mutable, keyAlgorithm)
