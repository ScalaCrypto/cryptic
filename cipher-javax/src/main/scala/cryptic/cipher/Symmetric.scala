package cryptic
package cipher

import java.security.SecureRandom
import java.security.spec.AlgorithmParameterSpec
import javax.crypto.{Cipher, KeyGenerator, SecretKey, SecretKeyFactory}
import javax.crypto.spec.{
  GCMParameterSpec,
  IvParameterSpec,
  PBEKeySpec,
  SecretKeySpec
}
import scala.util.Try

/** A trait that centralizes cryptographic operations relevant to symmetric
  * encryption and decryption, key generation, and key management.
  *
  * @tparam F
  *   A higher-kinded type representing the effect structure for computations
  *   involving cryptographic operations.
  */
trait Symmetric[F[_]]:
  import cryptic.default.{given, *}

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
  )(using Functor[F]): F[IArray[Byte]] =
    encrypt(plainText.bytes, plainText.aad, key, spec)

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
