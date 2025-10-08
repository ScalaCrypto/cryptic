package cryptic

object default extends Codec.Companion:
  export cryptic.Cryptic.{given, *}
  export cryptic.codec.default.{given, *}
  // Default is RsaAes
  export cryptic.crypto.RsaAes.{encrypt, decrypt}  // Only RsaAes given instances
  export cryptic.crypto.Rsa.newKeyPair  // Key generation function
