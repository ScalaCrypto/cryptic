package cryptic

import cryptic.serialization.Serializer

package object syntax:
  extension [V: Serializer](value: V)
    def encrypted(using encrypt: Encrypt): Encrypted[V] = Encrypted(value)
