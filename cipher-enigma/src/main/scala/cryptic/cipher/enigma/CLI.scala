package cryptic
package cipher
package enigma

import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Command-line interface for the Enigma encryption/decryption tool.
 *
 * Provides functionality to encrypt or decrypt messages using specific settings.
 * The input can be provided directly via text or read from a file.
 *
 * Arguments:
 *
 * - `-d, --decrypt`: Specifies decryption mode. Encrypt mode is the default.
 * - `-s, --settings SETTINGS`: Specifies the Enigma settings as a single string, e.g., "III-II-I AAA AAZ B ABCD".
 * This is optional if the environment variable `ENIGMA_SETTINGS` is set. This argument overrides the environment variable if provided.
 * - `-f, --file FILE`: Reads the input message from a specified file.
 * - `TEXT`: The input message text (optional if the input is provided via a file or stdin).
 *
 * Notes:
 *
 * - Input text is normalized to uppercase letters A–Z, ignoring non-alphabetical characters.
 * - When encrypting, the output is grouped into five-letter blocks.
 * - When decrypting, the output is returned as a continuous string.
 *
 * Methods:
 *
 * - `run`: Parses and processes command-line arguments for encryption or decryption.
 * - `main`: Entry point for the application. Accepts and processes command-line arguments.
 *
 * Helper Methods:
 *
 * - `usage`: Returns a string describing the CLI usage and options.
 * - `settings`: Parses and validates the Enigma machine settings.
 * - `process`: Determines encryption or decryption logic based on the settings.
 * - `encodePreamble`: Encrypts the input including the preamble.
 * - `decodePreamble`: Decrypts the input including the preamble.
 * - `encode`: Encrypts the input message.
 * - `decode`: Decrypts the input message.
 * - `input`: Handles input retrieval from text, file, or stdin.
 * - `readAllStdin`: Reads input from standard input.
 * - `readFile`: Reads input from a file.
 * - `reportError`: Logs and prints error messages.
 *
 * Extensions:
 *
 * - `group5`: String extension method to format output into groups of five letters.
 */
object CLI:
  private def usage(): String =
    """
      |Usage: Enigma [-d] [-s SETTINGS] [TEXT | -f FILE]
      |
      |  -d            Decrypt input (encryption is default)
      |  -s, --settings SETTINGS
      |                One string: "names rings [positions] reflector [plugpairs]"
      |                Optional if ENIGMA_SETTINGS is set in the environment. The flag overrides.
      |                Example: "III-II-I AAA AAZ B ABCD"
      |                If no positions are given a preamble will be generated when
      |                encrypting and is expected in the message when decrypting
      |  TEXT          Optional second argument containing the message text
      |  -f FILE       Read message text from FILE instead of TEXT or stdin
      |
      |Input text is normalized to letters A–Z (non-letters are ignored, lowercase allowed).
      |Encrypt output is grouped into five-letter groups. Decrypt output is a continuous string.
      |""".stripMargin

  import mainargs.{ParserForMethods, Flag, arg, main as m}
  import Enigma.default.given
  import Functor.tryFunctor

  @m(name = "enigma")
  def run(
      @arg(
        name = "decrypt",
        short = 'd',
        doc = "Decrypt input (default is encrypt)"
      ) decryptMode: Flag,
      @arg(
        name = "settings",
        short = 's',
        doc =
          "\"names rings [positions] reflector [plug-pairs]\" (can also be provided via ENIGMA_SETTINGS)"
      )
      settingsStr: Option[String],
      @arg(name = "f", short = 'f', doc = "Read message text from FILE")
      file: Option[String],
      @arg(doc = "Optional message TEXT (otherwise stdin)")
      text: String*
  ): Unit =

    val encryptMode = !decryptMode.value
    settings
      .map(process)
      .flatten
      .fold(reportError, println)

    def settings: Try[Settings] =
      settingsStr
        .orElse(sys.env.get("ENIGMA_SETTINGS"))
        .fold[Try[Settings]](
          Failure(
            new IllegalArgumentException(
              "Missing settings: use --settings or set ENIGMA_SETTINGS"
            )
          )
        )(Settings.parse)

    def process(settings: Settings): Try[String] =
      given s: Settings = settings
      (encryptMode, settings.preamble) match
        case (true, true)   => encodePreamble
        case (true, false)  => encode
        case (false, true)  => decodePreamble
        case (false, false) => decode

    def encodePreamble(using Settings): Try[String] =
      input.flatMap:
        _.encrypted
          .splitWith:
            case IArray(start, key, message) =>
              Success(
                (
                  start.glyph.string,
                  key.glyph.string,
                  message.glyph.string.group5
                )
              )
          .map:
            case (start, key, message) => s"$start $key $message"

    def decodePreamble(using Settings): Try[String] =
      input.flatMap:
        _.split(" ") match
          case Array(s, k, tail*) =>
            val start = s.glyph.iarray
            val key = k.glyph.iarray
            val message = tail.mkString.glyph.iarray
            Encrypted[Try, String](
              Success(CipherText(start, key, message))
            ).decrypted
          case _ =>
            Failure(
              new IllegalArgumentException(
                "Invalid input format: expected start and key"
              )
            )

    def encode(using Settings): Try[String] = decode.map(_.group5)

    def decode(using Settings): Try[String] =
      input
        .map(_.glyph)
        .map(Enigma.run)
        .map(_.string)

    def input: Try[String] =
      Try:
        file
          .map(readFile)
          .orElse(if text.nonEmpty then Some(text.mkString(" ")) else None)
          .getOrElse(readAllStdin())

    def readAllStdin(): String = Source.stdin.slurp

    def readFile(path: String): String = Source.fromFile(path).slurp

    def reportError(throwable: Throwable) =
      Console.err.println(throwable.getMessage)
      Console.err.println(usage())
      sys.exit(1)

  extension (s: String) def group5: String = s.grouped(5).mkString(" ")

  def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)
