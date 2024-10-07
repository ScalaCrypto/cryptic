package cryptic
package crypto

import java.nio.ByteBuffer
import java.security.SecureRandom
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{Cipher, SecretKey, SecretKeyFactory}

object AES {
  private val secureRandom = new SecureRandom()
  private val keyAlgorithm = "AES"
  case class AESParams(
      factoryAlgorithm: String = "PBKDF2WithHmacSHA256",
      mode: String = "CBC/PKCS5Padding",
      keyspecIterationCount: Int = 65536,
      keyspecLength: Int = 256,
      saltLength: Int = 32)

  case class Salt(bytes: Array[Byte]) extends AnyVal {
    def length: Int = bytes.length
  }
  case class AESPassphrase(value: String) extends AnyVal {
    def bytes: Array[Byte] = value.getBytes
  }
  private def newCipher(implicit aesParams: AESParams): Cipher = Cipher.getInstance(s"$keyAlgorithm/${aesParams.mode}")

  implicit def encrypt(implicit password: AESPassphrase, aesParams: AESParams): Encrypt = (plainText: PlainText) => {
    val salt = generateSalt(aesParams.saltLength)
    val key = keygen(password, salt)
    val cipher = newCipher(aesParams)
    cipher.init(Cipher.ENCRYPT_MODE, key)
    val params = cipher.getParameters
    val initVector = params.getParameterSpec(classOf[IvParameterSpec]).getIV
    val cipherText = cipher.doFinal(plainText)
    val ivLength = initVector.length
    val textLength = cipherText.length
    // Encode with length prefix to allow for backwards compatibility
    val buffer = ByteBuffer.allocate(12 + aesParams.saltLength + ivLength + textLength)
    buffer.putInt(aesParams.saltLength).put(salt.bytes).putInt(ivLength).put(initVector).putInt(textLength).put(cipherText)
    CipherText(buffer.array())
  }

  implicit def decrypt(implicit password: AESPassphrase, aesParams: AESParams): Decrypt = (cipherText: CipherText) => {
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
    val cipher = newCipher
    cipher.init(Cipher.DECRYPT_MODE, key, new IvParameterSpec(iv))
    Right[String, PlainText](PlainText(cipher.doFinal(text)))
  }

  def keygen(password: AESPassphrase, salt: Salt)(implicit aesParams: AESParams): SecretKey = {
    val factory = SecretKeyFactory.getInstance(aesParams.factoryAlgorithm)
    val keySpec = new PBEKeySpec(password.value.toCharArray, salt.bytes, aesParams.keyspecIterationCount, aesParams.keyspecLength)
    new SecretKeySpec(factory.generateSecret(keySpec).getEncoded, keyAlgorithm)
  }

  private def generateSalt(length: Int): Salt = {
    val salt = new Array[Byte](length)
    AES.secureRandom.nextBytes(salt)
    Salt(salt)
  }
}
