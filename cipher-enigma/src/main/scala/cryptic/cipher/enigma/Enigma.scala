package cryptic
package cipher
package enigma

import scala.util.{Success, Try}

case class Settings(rotors: Rotors, reflector: Reflector)

object Settings:
  def apply(start: Char): Settings = Settings(
    Rotors(
      RotorState(Rotor("I"), Glyph(0), start.glyph),
      RotorState(Rotor("II"), Glyph(0), Glyph(0)),
      RotorState(Rotor("III"), Glyph(0), Glyph(0))
    ),
    Reflector.B
  )

object Enigma:
  /** Version marker for future binary compatibility checks. */
  val version: Version = FixedVersion(0, 0, 0, 1)

  object default:
    // Expose common givens and types when using this cipher's default package
    export cryptic.default.{given, *}
    export Enigma.{given, *}

  given encrypt(using settings: Settings): Encrypt[Try] =
    (plaintext: PlainText) =>
      var rotors = settings.rotors
      val enc = plaintext.bytes
        .map(_.toChar)
        .filter(_.isGlyph)
        .map: c =>
          val g = c.glyph
          // Rotate all rotors applying ripple-carry rules
          rotors = rotors.rotate
          // Forward through rotors, reflect, then backward out
          val forward = rotors.in(g)
          val reflected = settings.reflector.reflect(forward)
          val output = rotors.out(reflected)
          output.char.toByte
      Success(CipherText(enc))
