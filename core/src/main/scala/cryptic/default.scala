package cryptic

/** Provides default cryptographic utilities and codec implementations.
 *
 * This object acts as a companion and consolidates commonly used cryptographic
 * constructs and utilities, enabling easy access to encoding, decoding,
 * encryption, and decryption functionalities.
 *
 * Exports:
 * - Implicit instances and utilities from `cryptic.Cryptic` for constructing
 * and operating on cryptographic computations.
 * - Default codec implementations from `cryptic.codec.default`.
 * - Encryption and decryption operations specific to the `RsaAes` cipher from
 * `cryptic.cipher.RsaAes`.
 * - Key pair generation utilities for RSA encryption from `cryptic.cipher.Rsa`.
 *
 * The `encrypt` and `decrypt` operations exposed in this object are limited
 * to the `RsaAes` cipher.
 */
object default extends Codec.Companion:
  export cryptic.Cryptic.{given, *}
  export cryptic.codec.default.{given, *}
  // Default is RsaAes
  export cryptic.cipher.RsaAes.{encrypt, decrypt}  // Only RsaAes given instances
  export cryptic.cipher.Rsa.newKeyPair  // Key generation function
