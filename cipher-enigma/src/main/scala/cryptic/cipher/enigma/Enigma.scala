package cryptic
package cipher
package enigma

import scala.util.{Failure, Success, Try}
import scala.io.Source

object Enigma:

  import Functor.tryFunctor

  object default:
    // Expose common givens and types when using this cipher's default package
    export cryptic.default.{given, *}
    export Enigma.{given, *}

  /** Encrypts a given plaintext into its cipher text representation based on
    * the provided settings.
    *
    * The method applies a series of operations including randomization of the
    * rotor start positions and key, running the encryption process on the
    * plaintext bytes, and constructing a `CipherText` object that represents
    * the encrypted message.
    *
    * @param settings
    *   The encryption settings, including rotor configuration and positions,
    *   that define the behavior of the encryption process.
    * @return
    *   An encryption `Encrypt` instance which uses `Try` as its effect type,
    *   producing a `Success` containing the cipher text upon successful
    *   encryption, or a `Failure` in case of unexpected issues.
    */
  given encrypt(using settings: Settings): Encrypt[Try] =
    (plaintext: PlainText) =>
      val n = settings.rotors.size
      val start = Glyph.random(n)
      val key = Glyph.random(n)
      val encryptedKey = run(key ++ key)(using settings.pos(start))
      val encryptedMessage = run(plaintext.bytes)(using settings.pos(key))
      val cipher =
        CipherText(start.iarray, encryptedKey.iarray, encryptedMessage.iarray)
      Success(cipher)

  /** Decrypts a given ciphertext into its plaintext representation using the
    * specified settings.
    *
    * This method extracts components from the ciphertext, including the rotor
    * start positions, encrypted key, and the encrypted message. It validates
    * the consistency of the key, and if valid, decrypts the message using the
    * rotor positions derived from the initial key. The result is returned as a
    * success or failure, depending on whether the decryption process completes
    * successfully.
    *
    * @param settings
    *   The decryption settings, including rotor configuration, reflector, and
    *   plugboard, which define the operations required for decryption. Note
    *   that the start positions are taken from the preamble
    * @return
    *   A `Decrypt` instance for the `Try` effect type, producing a `Success`
    *   containing the decrypted plaintext if the decryption process succeeds,
    *   or a `Failure` in case of any inconsistencies or errors.
    */
  given decrypt(using settings: Settings): Decrypt[Try] =
    (_: CipherText).splitWith:
      case IArray(startBytes, encryptedKeyBytes, encryptedMessageBytes) =>
        val n = settings.rotors.size
        val start = glyph(startBytes)
        val encryptedKey = glyph(encryptedKeyBytes)
        val encryptedMessage = glyph(encryptedMessageBytes)
        val doubleKey = run(encryptedKey)(using settings.pos(start))
        val (key1, key2) = doubleKey.splitAt(n)
        if key1 == key2
        then
          Failure(
            IllegalArgumentException(
              s"Inconsistent key in preamble ${start.string} ${doubleKey.string}"
            )
          )
        else
          val message = run(encryptedMessage)(using settings.pos(key1))
          Success(PlainText(message.string))

  def run(message: IArray[Byte])(using Settings): IArray[Glyph] = run(
    message.glyph
  )

  /** Processes an array of input glyphs and transforms each glyph based on the
    * provided settings, including rotor configurations, plugboard mappings, and
    * reflector behavior. The method simulates the operation of an Enigma-like
    * encryption machine.
    *
    * The transformation involves several steps for each input glyph:
    *   - Rotating rotors to their next positions.
    *   - Applying plugboard swaps to the glyph.
    *   - Passing the glyph through the rotors in forward direction.
    *   - Reflecting the glyph using the reflector.
    *   - Passing the glyph through the rotors in reverse direction.
    *   - Applying plugboard swaps to the glyph again.
    *
    * @param message
    *   The input array of glyphs to be processed by the encryption mechanism.
    * @param settings
    *   The encryption or transformation settings, including rotor
    *   configurations, reflector, and plugboard mappings, provided as an
    *   implicit parameter.
    * @return
    *   An array of glyphs resulting from the transformation of the input glyphs
    *   based on the specified settings.
    */
  def run(message: IArray[Glyph])(using settings: Settings): IArray[Glyph] =
    var rotors = settings.rotors
    message
      .map: g =>
        rotors = rotors.rotate
        val swappedIn = settings.plugboard.swap(g)
        val (forward, inTrace) = rotors.in(swappedIn)
        val reflected = settings.reflector.reflect(forward)
        val (output, outTrace) = rotors.out(reflected)
        val swappedOut = settings.plugboard.swap(output)
        scribe.trace(
          s"encoded ${g.char} ${inTrace.string}-${outTrace.string} ${swappedOut.char}"
        )
        swappedOut

  // ---- CLI ----
  private def usage(): String =
    """
      |Usage: Enigma [-d] "settings" [TEXT | -f FILE]
      |
      |  -d            Decrypt input (encryption is default)
      |  settings      One string: "names rings [positions] reflector [plugpairs]"
      |                Example: "III-II-I AAA AAZ B ABCD"  (A<->B, C<->D)
      |                If no positions are given a preamble will be generated when
      |                encrypting and is expected in the message when decrypting
      |  TEXT          Optional second argument containing the message text
      |  -f FILE       Read message text from FILE instead of TEXT or stdin
      |
      |Input text is normalized to letters A–Z (non-letters are ignored, lowercase allowed).
      |Encrypt output is grouped into five-letter groups. Decrypt output is a continuous string.
      |""".stripMargin

  private def readAllStdin(): String = Source.stdin.slurp

  private def readFile(path: String): String = Source.fromFile(path).slurp

  private def group5(s: String): String = s.grouped(5).mkString(" ")

  import mainargs.{ParserForMethods, Flag, arg, main as m}

  @m
  def run(
      @arg(
        name = "decrypt",
        short = 'd',
        doc = "Decrypt input (default is encrypt)"
      ) decrypt: Flag,
      @arg(
        name = "settings",
        doc = "\"names rings [positions] reflector [plug-pairs]\""
      )
      settingsStr: String,
      @arg(name = "f", short = 'f', doc = "Read message text from FILE")
      file: Option[String],
      @arg(doc = "Optional message TEXT (otherwise stdin)")
      text: String*
  ): Unit =
    val encryptMode = !decrypt.value

    val input: String =
      file
        .map(readFile)
        .orElse(if text.nonEmpty then Some(text.mkString(" ")) else None)
        .getOrElse(readAllStdin())

    import default.given
    import Functor.tryFunctor

    given settings: Settings = Settings(settingsStr)

    def encodePreamble =
      val enc = input.encrypted
      val (start, key, message) = enc
        .splitWith:
          case IArray(start, key, message) =>
            Success(
              (glyph(start).string, glyph(key).string, glyph(message).string)
            )
        .get
      s"$start $key ${group5(message)}"

    def decodePreamble =
      val arr = input.split(" ")
      val start = arr(0).glyph
      val key = arr(1).glyph
      val message = arr.drop(2).mkString.glyph
      val enc = Encrypted[Try, String](
        Success(CipherText(start.iarray, key.iarray, message.iarray))
      )
      enc.decrypted.get

    def encode = group5(run(input.glyph).string)

    def decode = run(input.glyph).string

    val result = (encryptMode, settings.preamble) match
      case (true, true)   => encodePreamble
      case (true, false)  => encode
      case (false, true)  => decodePreamble
      case (false, false) => decode

    println(result)

  def main(args: Array[String]): Unit =
    ParserForMethods(Enigma).runOrExit(args)
