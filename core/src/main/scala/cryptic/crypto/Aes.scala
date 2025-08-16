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

/** Object AES provides encryption and decryption utilities using the AES
  * algorithm. The functions provided allow encryption of plaintext into
  * ciphertext and decryption of ciphertext back into plaintext using a
  * passphrase and various parameters defined in the AESParams case class.
  */
object Aes:
  private val secureRandom = new SecureRandom()
  val keyAlgorithm = "AES"

  trait AesParams:
    val factoryAlgorithm: String
    val mode: String
    val keyspecIterationCount: Int
    val keyspecLength: Int
    val saltLength: Int
    val ivLength: Int
    def paramSpec(iv: Array[Byte]): AlgorithmParameterSpec
    def newIv(): Array[Byte] =
      val iv = new Array[Byte](ivLength)
      secureRandom.nextBytes(iv)
      iv

  case class CbcParams(
      factoryAlgorithm: String = "PBKDF2WithHmacSHA256",
      mode: String = "CBC/PKCS5Padding",
      keyspecIterationCount: Int = 310000,
      keyspecLength: Int = 256,
      saltLength: Int = 32,
      ivLength: Int = 16, // 16-byte IV (128 bits) for CBC mode
      version: Int = 1
  ) extends AesParams:
    private val validKeyspecLenghts = List(128, 192, 256)
    require(
      validKeyspecLenghts.contains(keyspecLength),
      s"Invalid keyspecLength: $keyspecLength. Allowed values are ${validKeyspecLenghts.mkString(", ")}."
    )
    override def paramSpec(iv: Array[Byte]): IvParameterSpec =
      new IvParameterSpec(iv)

  case class GcmParams(
      factoryAlgorithm: String = "PBKDF2WithHmacSHA256",
      mode: String = "GCM/NoPadding",
      keyspecIterationCount: Int = 310000,
      keyspecLength: Int = 256,
      saltLength: Int = 32,
      version: Int = 1
  ) extends AesParams:
    require(
      keyspecIterationCount >= 310000,
      "Iteration count too low for security"
    )
    require(keyspecLength >= 256, "Key length must be at least 256 bits")
    require(saltLength >= 16, "Salt length must be at least 16 bytes")
    private val GCM_TAG_LENGTH = 128 // GCM authentication tag length in bits
    override val ivLength: Int = 12
    override def paramSpec(iv: Array[Byte]): AlgorithmParameterSpec =
      new GCMParameterSpec(GCM_TAG_LENGTH, iv)

  case class Salt(bytes: IArray[Byte]) extends AnyVal:
    def length: Int = bytes.length

  object Salt:
    def apply(length: Int): Salt =
      val salt = new Array[Byte](length)
      Aes.secureRandom.nextBytes(salt)
      Salt(salt.immutable)

  /** Case class AESPassphrase for handling cryptographic passphrases.
    *
    * @param bytes
    *   Array of bytes representing the passphrase.
    */
  case class AesPassphrase(bytes: IArray[Byte]):
    def chars: IArray[Char] = bytes.map(_.toChar)
    override def toString: String = new String(bytes.mutable)

  object AesPassphrase:
    def apply(password: String): AesPassphrase = new AesPassphrase(
      password.getBytes.immutable
    )
    def apply(n: Int): AesPassphrase =
      val array = new Array[Byte](n)
      secureRandom.nextBytes(array)
      apply(array.immutable)

  private def newCipher(mode: String): Cipher =
    Cipher.getInstance(s"$keyAlgorithm/${mode}")

  given encrypt(using
      passphrase: AesPassphrase,
      aesParams: AesParams
  ): Encrypt = (plainText: PlainText) =>
    val salt = Salt(aesParams.saltLength)
    val key = keygen(passphrase, salt)
    val iv = aesParams.newIv()
    val ivSpec = aesParams.paramSpec(iv)
    val cipherText = encrypt(plainText.bytes, key, ivSpec, aesParams.mode)
    CipherText(
      plainText.manifest,
      salt.bytes,
      iv.immutable,
      cipherText
    )

  def encrypt(
      bytes: IArray[Byte],
      key: SecretKey,
      ivSpec: AlgorithmParameterSpec,
      mode: String
  ): IArray[Byte] =
    val cipher = newCipher(mode)
    cipher.init(Cipher.ENCRYPT_MODE, key, ivSpec)
    cipher.doFinal(bytes.mutable).immutable

  given decrypt(using
      passphrase: AesPassphrase,
      aesParams: AesParams
  ): Decrypt = (cipherText: CipherText) =>
    Try:
      val IArray(manifest, salt, iv, bytes) = cipherText.split
      val key = keygen(passphrase, Salt(salt))
      val ivSpec = aesParams.paramSpec(iv.mutable)
      val decrypted = decrypt(bytes, key, ivSpec, aesParams.mode)
      PlainText(decrypted, manifest)

  def decrypt(
      bytes: IArray[Byte],
      key: SecretKey,
      ivSpec: AlgorithmParameterSpec,
      mode: String
  ): IArray[Byte] =
    val cipher = newCipher(mode)
    cipher.init(Cipher.DECRYPT_MODE, key, ivSpec)
    cipher.doFinal(bytes.mutable).immutable

  /** Generates a secret key using the provided password and salt.
    *
    * @param passphrase
    *   the passphrase used to generate the key
    * @param salt
    *   the salt value used in the key generation process
    * @param aesParams
    *   the given AES parameters containing the factory algorithm, key
    *   specification iteration count, and key specification length
    * @return
    *   the generated secret key
    */
  def keygen(passphrase: AesPassphrase, salt: Salt)(using
      aesParams: AesParams
  ): SecretKey =
    val factory = SecretKeyFactory.getInstance(aesParams.factoryAlgorithm)
    val keySpec =
      new PBEKeySpec(
        passphrase.chars.mutable,
        salt.bytes.mutable,
        aesParams.keyspecIterationCount,
        aesParams.keyspecLength
      )
    new SecretKeySpec(
      factory.generateSecret(keySpec).getEncoded,
      keyAlgorithm
    )

  def keygen(size: Int = 256): SecretKey =
    val keyGenerator = KeyGenerator.getInstance("AES")
    keyGenerator.init(size)
    keyGenerator.generateKey()

  def key(bytes: IArray[Byte]): SecretKey =
    new SecretKeySpec(bytes.mutable, keyAlgorithm)
