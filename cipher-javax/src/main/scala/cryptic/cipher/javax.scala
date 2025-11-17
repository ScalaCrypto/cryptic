package cryptic
package cipher

import java.security.SecureRandom
import javax.crypto.{KeyGenerator, SecretKey, SecretKeyFactory}
import javax.crypto.spec.{PBEKeySpec, SecretKeySpec}

val secureRandom: SecureRandom =
  val preferredAlgorithms = List("NativePRNGNonBlocking", "SHA1PRNG")
  val sr = preferredAlgorithms.view
    .map(alg => scala.util.Try(SecureRandom.getInstance(alg)))
    .collectFirst { case scala.util.Success(sr) => sr }
    .getOrElse(new SecureRandom())
  sr.nextBytes(new Array[Byte](20)) // Force initial seeding
  sr

extension (secureRandom: SecureRandom)
  def newBytes(length: Int): IArray[Byte] =
    val arr = new Array[Byte](length)
    secureRandom.nextBytes(arr)
    arr.immutable

extension (saltObject: Salt.type)
  def apply(length: Int): Salt = Salt(secureRandom.newBytes(length))

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
def keygen(passphrase: Passphrase, salt: Salt, factoryAlgorithm: String, iterationCount: Int, specLength: Int, keyAlgorithm: String): SecretKey =
  val factory = SecretKeyFactory.getInstance(factoryAlgorithm)
  val keySpec =
    new PBEKeySpec(
      passphrase.chars.mutable,
      salt.bytes.mutable,
      iterationCount,
      specLength
    )
  new SecretKeySpec(
    factory.generateSecret(keySpec).getEncoded,
    keyAlgorithm
  )

def keygen(size: Int = 256, algorithm: String): SecretKey =
  val keyGenerator = KeyGenerator.getInstance(algorithm)
  keyGenerator.init(size)
  keyGenerator.generateKey()

def key(bytes: IArray[Byte], algorithm: String): SecretKey =
  new SecretKeySpec(bytes.mutable, algorithm)
