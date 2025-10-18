package cryptic

/** Default, batteries-included API surface for cryptic.
  *
  * Mixes in Codec syntax and re-exports common givens and utilities so that
  * users can `import cryptic.default.*` to get started quickly.
  * The default crypto backend is RsaAes.
  */
object default extends Codec.Companion:
  export cryptic.Cryptic.{given, *}
  export cryptic.codec.default.{given, *}
  // Default is RsaAes
  export cryptic.crypto.RsaAes.{encrypt, decrypt}  // Only RsaAes given instances
  export cryptic.crypto.Rsa.newKeyPair  // Key generation function
