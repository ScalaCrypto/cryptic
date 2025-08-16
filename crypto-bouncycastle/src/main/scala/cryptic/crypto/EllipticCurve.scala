package cryptic
package crypto

import cryptic.{CipherText, Decrypt, Encrypt, PlainText}
import org.bouncycastle.jce.provider.BouncyCastleProvider

import java.security.*
import javax.crypto.Cipher
import scala.util.Try

/** Elliptic Curve Integrated Encryption Scheme depends on private and public
  * keys. The public key should be given for encryption and the private key for
  * decryption
  */
object EllipticCurve extends Asymmetric:
  Security.addProvider(new BouncyCastleProvider())

  def newCipher(): Cipher = Cipher.getInstance("ECIES", "BC")

  def newKeyPairGenerator(): KeyPairGenerator =
    KeyPairGenerator.getInstance("EC")
