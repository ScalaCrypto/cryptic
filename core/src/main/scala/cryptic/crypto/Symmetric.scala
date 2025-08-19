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

trait Symmetric:
  import Symmetric.*
  export Symmetric.*

  def factoryAlgorithm: String
  def keyAlgorithm: String
  def keyspecIterationCount: Int
  def keyspecLength: Int

  def newCipher(
      mode: Int,
      key: SecretKey,
      spec: AlgorithmParameterSpec
  ): Cipher

  def encrypt(
      bytes: IArray[Byte],
      key: SecretKey,
      spec: AlgorithmParameterSpec
  ): IArray[Byte] =
    val cipher = newCipher(Cipher.ENCRYPT_MODE, key, spec)
    cipher.doFinal(bytes.mutable).immutable

  def decrypt(
      bytes: IArray[Byte],
      key: SecretKey,
      spec: AlgorithmParameterSpec
  ): IArray[Byte] =
    val cipher = newCipher(Cipher.DECRYPT_MODE, key, spec)
    cipher.doFinal(bytes.mutable).immutable

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

    def apply(n: Int): Passphrase =
      val array = new Array[Byte](n)
      secureRandom.nextBytes(array)
      apply(array.immutable)

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
  val secureRandom = new SecureRandom()

  case class Salt(bytes: IArray[Byte]) extends AnyVal:
    def length: Int = bytes.length

  object Salt:
    def apply(length: Int): Salt =
      val salt = new Array[Byte](length)
      secureRandom.nextBytes(salt)
      Salt(salt.immutable)
