package object cryptic {
  type CipherText = String
  object CipherText {
    def apply(x: Any): CipherText = String.valueOf(x)
  }
}