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
object Aes extends Symmetric[Try]:
  val transformation: String = "GCM/NoPadding"
  val saltLength: Int = 32
  val gcmTagLength = 128 // GCM authentication tag length in bits
  val ivLength: Int = 12

  override val factoryAlgorithm: String = "PBKDF2WithHmacSHA256"
  override val keyAlgorithm = "AES"
  override val keyspecIterationCount: Int = 310000
  override val keyspecLength: Int = 256

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
        val key = keygen(passphrase, salt)
        val iv = newIv()
        val ivSpec = paramSpec(iv)
        encrypt(plainText.bytes, plainText.aad, key, ivSpec).map: encrypted =>
          CipherText(
            plainText.aad,
            salt.bytes,
            iv,
            encrypted
          )
      .flatten

  given decrypt(using passphrase: Passphrase): Decrypt[Try] =
    (cipherText: CipherText) =>
      Try:
        val IArray(aad, salt, iv, bytes) = cipherText.split
        val key = keygen(passphrase, Salt(salt))
        val ivSpec = paramSpec(iv)
        decrypt(bytes, aad, key, ivSpec).map(PlainText(_, aad))
      .flatten
