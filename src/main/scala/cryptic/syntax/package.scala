package cryptic

import serialization.Serializer

package object syntax {
  implicit class RichAny[V: Serializer](val value: V) {
    def encrypted(implicit encrypt: Encrypt): Encrypted[V] =
      Encrypted(value)
  }
}
