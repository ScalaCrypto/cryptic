package cryptic
package crypto

import java.security.SecureRandom
import java.security.spec.AlgorithmParameterSpec
import javax.crypto.spec.{
  GCMParameterSpec,
  IvParameterSpec,
  PBEKeySpec,
  SecretKeySpec
}
import javax.crypto.{Cipher, KeyGenerator, SecretKey, SecretKeyFactory}
import scala.util.Try

/** A trait that centralizes cryptographic operations relevant to symmetric
 * encryption and decryption, key generation, and key management.
 *
 * @tparam F
 * A higher-kinded type representing the effect structure for computations
 * involving cryptographic operations.
 */
trait Symmetric[F[_]]:
  import Symmetric.*
  export Symmetric.*

  /** SecretKeyFactory algorithm used for PBKDF2 or similar derivation. */
  def factoryAlgorithm: String

  /** JCA key algorithm name (e.g., "AES"). */
  def keyAlgorithm: String

  /** Iteration count used when deriving keys from passphrases. */
  def keyspecIterationCount: Int

  /** Derived key length in bits. */
  def keyspecLength: Int

  def newCipher(
      mode: Int,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  ): Cipher

  def encrypt(
      bytes: IArray[Byte],
      aad: AAD,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  )(using Functor[F]): F[IArray[Byte]] =
    Try:
      val cipher = newCipher(Cipher.ENCRYPT_MODE, key, spec)
      if aad.nonEmpty then cipher.updateAAD(aad.mutable)
      cipher.doFinal(bytes.mutable).immutable
    .lift

  def encrypt(
      plainText: PlainText,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  )(using Functor[F]): F[IArray[Byte]] = encrypt(plainText.bytes, plainText.aad, key, spec)

  def decrypt(
      bytes: IArray[Byte],
      aad: AAD,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  )(using Functor[F]): F[IArray[Byte]] =
    Try:
      val cipher = newCipher(Cipher.DECRYPT_MODE, key, spec)
      if aad.nonEmpty then cipher.updateAAD(aad.mutable)
      cipher.doFinal(bytes.mutable).immutable
    .lift

  /** Case class Passphrase for handling cryptographic passphrases.
    *
    * @param bytes
    *   Array of bytes representing the passphrase.
    */
  case class Passphrase(bytes: IArray[Byte]):
    def chars: IArray[Char] = bytes.map(_.toChar)

    override def toString: String = new String(bytes.mutable)

  object Passphrase:
    def apply(password: String): Passphrase = new Passphrase(
      password.getBytes.immutable
    )

    /** Generates a Passphrase by creating a random array of bytes of the
      * specified length.
      *
      * @param n
      *   the number of random bytes to generate for the Passphrase
      * @return
      *   a Passphrase instance created from the generated random bytes
      */
    def apply(n: Int): Passphrase =
      val bytes = secureRandom
        .newBytes(n)
        .map(b => (b & 63 + 32).toByte)
        .mutable // Map to ASCII printable chars
      val firstLast =
        secureRandom.newBytes(n).map(b => (b & 63 + 33).toByte) // No space
      bytes(0) = firstLast(0)
      bytes(bytes.length - 1) = firstLast(1)
      apply(bytes.immutable)

  /** Generates a secret key using the provided password and salt.
    *
    * @param passphrase
    *   the passphrase used to generate the key
    * @param salt
    *   the salt value used in the key generation process
    * @return
    *   the generated secret key
    */
  def keygen(passphrase: Passphrase, salt: Salt): SecretKey =
    val factory = SecretKeyFactory.getInstance(factoryAlgorithm)
    val keySpec =
      new PBEKeySpec(
        passphrase.chars.mutable,
        salt.bytes.mutable,
        keyspecIterationCount,
        keyspecLength
      )
    new SecretKeySpec(
      factory.generateSecret(keySpec).getEncoded,
      keyAlgorithm
    )

  def keygen(size: Int = 256): SecretKey =
    val keyGenerator = KeyGenerator.getInstance(keyAlgorithm)
    keyGenerator.init(size)
    keyGenerator.generateKey()

  def key(bytes: IArray[Byte]): SecretKey =
    new SecretKeySpec(bytes.mutable, keyAlgorithm)

object Symmetric:

  case class Salt(bytes: IArray[Byte]) extends AnyVal:
    def length: Int = bytes.length

  object Salt:
    def apply(length: Int): Salt =
      Salt(secureRandom.newBytes(length))
