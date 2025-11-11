package cryptic
package cipher

/** The `default` object provides a convenient re-export interface for RSA and
 * RSA-AES related cryptographic operations. It acts as a streamlined access
 * point by aggregating commonly used functionalities related to encryption,
 * decryption, and key generation from `RsaAes` and `Rsa` objects.
 *
 * This object includes:
 *
 *   - Re-exported type classes and methods from `RsaAes`, such as encryption
 *     and decryption capabilities that utilize a combination of RSA and AES
 *     algorithms.
 *   - A direct export of the `Rsa.newKeyPair` function, enabling seamless
 *     creation of RSA key pairs.
 *
 * The `given` and `export` directives simplify usage by reducing the need for
 * explicit imports from `RsaAes` and `Rsa`.
 */
object default:
  export RsaAes.{given, *}
  export Rsa.newKeyPair
