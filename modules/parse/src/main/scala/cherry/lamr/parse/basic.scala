package cherry.lamr.parse

package basic

import cats.parse.{Numbers, Parser, Parser0}
import Parser.*
import cats.parse.Numbers.signedIntString
import cherry.lamr.{BuiltinType, Lang, RecordKey}
import cherry.fix.Fix

val lowerLetter = charIn('a' to 'z')
val upperLetter = charIn('A' to 'Z')
val underscore  = char('_')
val digit       = charIn('0' to '9')
val nonZero     = charIn('1' to '9')
val zero        = char('0')

val letter        = lowerLetter orElse upperLetter orElse underscore
val letterOrDigit = letter orElse digit

val identifier = (letter *> letterOrDigit.rep0).string

val frac: Parser[Any] = Parser.char('.') ~ digit
val exp: Parser[Unit] = (Parser.charIn("eE") ~ Parser.charIn("+-").? ~ digit).void

val signedIntString: Parser[String] =
  (Parser.char('-').?.soft.with1 ~ Numbers.nonNegativeIntString).string

val bigInt: Parser[BigInt] = signedIntString.map(BigInt(_))

val integer = bigInt
val float   = (signedIntString ~ (frac | exp)).string.mapFilter(_.toDoubleOption)
val bool    = (string("true") as true) | (string("false") as false)
val str     = char('"') *> identifier <* char('"')

val builtinType =
  oneOf(
    List(
      "str"   -> BuiltinType.Str,
      "int"   -> BuiltinType.Integer,
      "bool"  -> BuiltinType.Bool,
      "float" -> BuiltinType.Float,
    ).map((name, bt) => string(name) as bt)
  )

val whitespace = charIn(' ', '\t', '\n').rep0.void.backtrack

val symbolKey = identifier.map(RecordKey.Symbol(_))

extension [A](p: Parser[A]) def spaced: Parser[A] = p.surroundedBy(whitespace)

extension [A](p: Parser0[A]) def spaced0: Parser0[A] = p.surroundedBy(whitespace)
