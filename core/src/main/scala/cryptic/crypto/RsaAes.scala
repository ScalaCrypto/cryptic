package cryptic
package crypto

/** RsaAse object provides encryption, decryption, and key generation
  * functionalities using the RSA algorithm to create an AES key that is
  * encrypted using RSA and stored together with the encrypted PlainText.
  *
  * @define encrypt
  *   Performs encryption on the given plain text using the provided public key.
  * @define decrypt
  *   Performs decryption on the given cipher text using the provided private
  *   key.
  * @define keygen
  *   Generates an RSA key pair with the specified size.
  */
object RsaAes extends AsymmetricAes:
  given rsa: Asymmetric = Rsa
