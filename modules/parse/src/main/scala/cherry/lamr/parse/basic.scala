package cherry.lamr.parse

package basic

import cats.parse.Parser
import Parser._
import tofu.syntax.monadic._
import cherry.lamr.Lang
import cherry.fix.Fix
import cherry.lamr.RecordKey

val lowerLetter = charIn('a' to 'z')
val upperLetter = charIn('A' to 'Z')
val underscore  = char('_')
val digit       = charIn('0' to '9')
val nonZero     = charIn('1' to '9')
val zero        = char('0')

val letter        = lowerLetter orElse upperLetter orElse underscore
val letterOrDigit = letter orElse digit

val identifier = (letter *> letterOrDigit.rep0).string

val positiveInt = (nonZero *> digit.rep0).string.map(BigInt(_))
val negativeInt = (char('-') *> positiveInt).map(-_)
val zeroInt     = char('0') as BigInt(0)

val integer = zeroInt orElse positiveInt orElse negativeInt

val whitespace = charIn(' ', '\t', '\n').rep0.void

extension [A](p: Parser[A]) def spaced: Parser[A] = p.surroundedBy(whitespace)