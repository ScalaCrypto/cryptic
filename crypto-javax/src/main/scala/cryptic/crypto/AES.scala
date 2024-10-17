package cryptic
package crypto

import java.nio.ByteBuffer
import java.security.SecureRandom
import javax.crypto.{Cipher, SecretKey, SecretKeyFactory}
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import scala.util.Try

/**
 * Object AES provides encryption and decryption utilities using the AES algorithm. The functions provided allow
 * encryption of plaintext into ciphertext and decryption of ciphertext back into plaintext using a passphrase and
 * various parameters defined in the AESParams case class.
 */
object AES {
  private val secureRandom = new SecureRandom()
  private val keyAlgorithm = "AES"
  case class AESParams(
      factoryAlgorithm: String = "PBKDF2WithHmacSHA256",
      mode: String = "CBC/PKCS5Padding",
      keyspecIterationCount: Int = 65536,
      keyspecLength: Int = 256,
      saltLength: Int = 32) {
    private val validKeyspecLenghts = List(128, 192, 256)
    assert(
      validKeyspecLenghts.contains(keyspecLength),
      s"Invalid keyspecLength: $keyspecLength. Allowed values are ${validKeyspecLenghts.mkString(", ")}.")
  }

  case class Salt(bytes: Array[Byte]) extends AnyVal {
    def length: Int = bytes.length
  }
  /**
   * Case class AESPassphrase for handling cryptographic passphrases.
   *
   * @param bytes Array of bytes representing the passphrase.
   */
  case class AESPassphrase(bytes: Array[Byte]) {
    def chars: Array[Char] = bytes.map(_.toChar)
    override def toString: String = new String(bytes)
  }

  object AESPassphrase {
    def apply(password: String): AESPassphrase = new AESPassphrase(password.getBytes)
  }

  private def newCipher(implicit aesParams: AESParams): Cipher = Cipher.getInstance(s"$keyAlgorithm/${aesParams.mode}")

  implicit def encrypt(implicit passphrase: AESPassphrase, aesParams: AESParams): Encrypt = (plainText: PlainText) => {
    val salt = generateSalt(aesParams.saltLength)
    val key = keygen(passphrase, salt)
    val cipher = newCipher(aesParams)
    cipher.init(Cipher.ENCRYPT_MODE, key)
    val params = cipher.getParameters
    val initVector = params.getParameterSpec(classOf[IvParameterSpec]).getIV
    val cipherText = cipher.doFinal(plainText)
    val ivLength = initVector.length
    val textLength = cipherText.length
    // Encode with length prefix to allow for backwards compatibility
    val buffer = ByteBuffer.allocate(12 + aesParams.saltLength + ivLength + textLength)
    buffer
      .putInt(aesParams.saltLength)
      .put(salt.bytes)
      .putInt(ivLength)
      .put(initVector)
      .putInt(textLength)
      .put(cipherText)
    CipherText(buffer.array())
  }

  implicit def decrypt(implicit passphrase: AESPassphrase, aesParams: AESParams): Decrypt = (cipherText: CipherText) =>
    {
      val buffer = ByteBuffer.wrap(cipherText.bytes)
      def getNextBytes = {
        val nextBuffer = new Array[Byte](buffer.getInt())
        buffer.get(nextBuffer)
        nextBuffer
      }
      val salt: Array[Byte] = getNextBytes
      val key = keygen(passphrase, Salt(salt))
      val iv: Array[Byte] = getNextBytes
      val text: Array[Byte] = getNextBytes
      val cipher = newCipher
      cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv))
      Try[PlainText](PlainText(cipher.doFinal(text)))
    }

  /**
   * Generates a secret key using the provided password and salt.
   *
   * @param passphrase
   *   the passphrase used to generate the key
   * @param salt
   *   the salt value used in the key generation process
   * @param aesParams
   *   implicit AES parameters containing the factory algorithm, key specification iteration count, and key
   *   specification length
   * @return
   *   the generated secret key
   */
  def keygen(passphrase: AESPassphrase, salt: Salt)(implicit aesParams: AESParams): SecretKey = {
    val factory = SecretKeyFactory.getInstance(aesParams.factoryAlgorithm)
    val keySpec =
      new PBEKeySpec(passphrase.chars, salt.bytes, aesParams.keyspecIterationCount, aesParams.keyspecLength)
    val key = new SecretKeySpec(factory.generateSecret(keySpec).getEncoded, keyAlgorithm)
    key
  }

  private def generateSalt(length: Int): Salt = {
    val salt = new Array[Byte](length)
    AES.secureRandom.nextBytes(salt)
    Salt(salt)
  }
}
