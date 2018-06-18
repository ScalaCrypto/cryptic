package cryptic
package crypto

import java.nio.ByteBuffer
import java.security.SecureRandom

import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{Cipher, SecretKey, SecretKeyFactory}

object AES {
  private val secureRandom = new SecureRandom()
  private val factoryAlgorithm = "PBKDF2WithHmacSHA256"
  private val keyAlgorithm = "AES"
  private val keyspecIterationCount = 65536
  private val keyspecLength = 256
  private val saltLength = 32

  case class Salt(bytes: Array[Byte]) extends AnyVal {
    def length: Int = bytes.length
  }
  case class AESPassword(value: String) extends AnyVal {
    def bytes: Array[Byte] = value.getBytes
  }

  val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")

  implicit def encrypt(implicit password: AESPassword): Encrypt = (plainText: PlainText) => {
    import javax.crypto.Cipher
    import javax.crypto.spec.IvParameterSpec
    val salt = generateSalt
    val key = keygen(password, salt)
    cipher.init(Cipher.ENCRYPT_MODE, key)
    val params = cipher.getParameters
    val initVector = params.getParameterSpec(classOf[IvParameterSpec]).getIV
    val saltLength = salt.length
    val cipherText = cipher.doFinal(plainText)
    val ivLength = initVector.length
    val textLength = cipherText.length
    // Encode with length prefix to allow for backwards compatibility
    val buffer = ByteBuffer.allocate(12 + saltLength + ivLength + textLength)
    buffer
      .putInt(saltLength).put(salt.bytes)
      .putInt(ivLength).put(initVector)
      .putInt(textLength).put(cipherText)
    CipherText(buffer.array())
  }

  implicit def decrypt(implicit password: AESPassword): Decrypt = (cipherText: CipherText) => {
    val buffer = ByteBuffer.wrap(cipherText.bytes)
    def getNextBytes = {
      val nextBuffer = new Array[Byte](buffer.getInt())
      buffer.get(nextBuffer)
      nextBuffer
    }
    val salt: Array[Byte] = getNextBytes
    val key = keygen(password, Salt(salt))
    val iv: Array[Byte] = getNextBytes
    val text: Array[Byte] = getNextBytes
    cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv))
    Right[String, PlainText](PlainText(cipher.doFinal(text)))
  }

  def keygen(password: AESPassword, salt: Salt): SecretKey = {
    val factory = SecretKeyFactory.getInstance(factoryAlgorithm)
    val keySpec = new PBEKeySpec(password.value.toCharArray, salt.bytes, keyspecIterationCount, keyspecLength)
    new SecretKeySpec(factory.generateSecret(keySpec).getEncoded, keyAlgorithm)
  }

  private def generateSalt: Salt = {
    val salt = new Array[Byte](saltLength)
    AES.secureRandom.nextBytes(salt)
    Salt(salt)
  }
}