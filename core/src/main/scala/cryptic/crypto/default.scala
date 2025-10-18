package cryptic
package crypto

/** Convenience re-exports for the default crypto backend (RsaAes). */
object default:
  export RsaAes.{given, *}
  export Rsa.newKeyPair
