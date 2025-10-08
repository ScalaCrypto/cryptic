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
object Aes extends Symmetric:
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

  def newIv(): Array[Byte] =
    val iv = new Array[Byte](ivLength)
    secureRandom.nextBytes(iv)
    iv

  def paramSpec(iv: Array[Byte]): AlgorithmParameterSpec =
    new GCMParameterSpec(gcmTagLength, iv)

  given encrypt(using
      passphrase: Passphrase
  ): Encrypt[Try] =
    (plainText: PlainText) =>
      Try:
        val salt = Salt(saltLength)
        val key = keygen(passphrase, salt)
        val iv = newIv()
        val ivSpec = paramSpec(iv)
        val cipherText = encrypt(plainText.bytes, key, ivSpec)
        CipherText(
          plainText.manifest,
          salt.bytes,
          iv.immutable,
          cipherText
        )

  given decrypt(using
      passphrase: Passphrase
  ): Decrypt[Try] = (cipherText: CipherText) =>
    Try:
      val IArray(manifest, salt, iv, bytes) = cipherText.split
      val key = keygen(passphrase, Salt(salt))
      val ivSpec = paramSpec(iv.mutable)
      val decrypted = decrypt(bytes, key, ivSpec)
      PlainText(decrypted, manifest)
