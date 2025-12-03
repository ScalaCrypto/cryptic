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

case class PBEKeyParams(
    factoryAlgorithm: String,
    iterationCount: Int,
    keyLength: Int,
    keyAlgorithm: String
):
  def keySpec(passphrase: Passphrase, salt: Salt): PBEKeySpec =
    new PBEKeySpec(
      passphrase.chars.mutable,
      salt.bytes.mutable,
      iterationCount,
      keyLength
    )
  def bytes: IArray[Byte] =
    IArray.join(
      factoryAlgorithm.bytes,
      iterationCount.bytes,
      keyLength.bytes,
      keyAlgorithm.bytes
    )

object PBEKeyParams:
  def apply(bytes: IArray[Byte]): PBEKeyParams = bytes.split match
    case IArray(fa, ic, kl, ka) =>
      PBEKeyParams(fa.string, ic.int, kl.int, ka.string)

extension (passphrase: Passphrase)
  /** Derives a secret key using the provided salt and password-based encryption
    * (PBE) parameters.
    *
    * @param salt
    *   The cryptographic salt used in conjunction with the password for key
    *   derivation.
    * @param pbeKeyParams
    *   Implicit parameter providing the settings for key derivation, including
    *   the factory algorithm, iteration count, key length, and key algorithm.
    * @return
    *   A derived secret key based on the provided parameters, suitable for
    *   cryptographic operations.
    */
  def secretKey(salt: Salt)(using pbeKeyParams: PBEKeyParams): SecretKey =
    val keySpec = pbeKeyParams.keySpec(passphrase, salt)
    val factory = SecretKeyFactory.getInstance(pbeKeyParams.factoryAlgorithm)
    new SecretKeySpec(
      factory.generateSecret(keySpec).getEncoded,
      pbeKeyParams.keyAlgorithm
    )

extension (PassphraseObject: Passphrase.type)
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
