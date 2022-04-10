package cherry.lamr.parse

package term

import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.*
import cherry.fix.Fix
import cherry.lamr.{Lang, RecordKey}
import cherry.lamr.norm.Term
import cherry.lamr.parse.basic.{*, given}
import cherry.lamr.parse.types.typeTerm
import tofu.syntax.monadic.*

val term: Parser[Term] = defer(theTerm)

val separator = char(',')

val listSyntax = term.spaced
  .repSep0(separator)
  .map(_.iterator.zipWithIndex.map { (t, i) => Lang.set(RecordKey.Index(i), t) })
  .map(mergeAll)

val symbolTerm: Parser[Fix[Lang]] =
  ((symbolKey <* whitespace) ~ (char('=') *> whitespace *> term).?).map {
    case (key, None)    => Lang.get(key)
    case (key, Some(t)) => Lang.set(key, t)
  }

val listTerm = char('[') *> listSyntax <* char(']')

def mergeAll(terms: IterableOnce[Fix[Lang]]): Fix[Lang] =
  terms.iterator.reduceOption(Lang.Merge(_, _).fix).getOrElse(Lang.Unit)

val assignment = (symbolKey, char('=').spaced *> term).mapN((key, t) => Lang.set(key, t))

val recordElement = assignment.backtrack.eitherOr(term)

def indexRecord(elems: NonEmptyList[Either[Term, Term]]): Iterator[Term] =
  elems.iterator
    .scanLeft((0, Lang.Unit: Term)) {
      case ((i, _), Right(t)) => (i, t)
      case ((i, _), Left(t))  => (i + 1, Lang.set(i, t))
    }
    .map(_._2)
    .drop(1)

val recordTail = separator *> whitespace *> (recordElement <* whitespace).repSep0(separator *> whitespace)

val builtin = char('$') *> (builtinType.map(Lang.Builtin(_)) | bool.map(Lang.Bool(_)))

val parenElements =
  (whitespace *> (recordElement.spaced ~ recordTail.?).?).map {
    case None                     => Lang.Unit
    case Some((Left(only), None)) => only
    case Some((head, tail))       => mergeAll(indexRecord(NonEmptyList(head, tail.getOrElse(Nil))))
  }

val recordTerm = char('(') *> parenElements <* char(')')

val integerTerm = integer.map(Lang.Integer(_))

val floatTerm = float.map(Lang.Float(_))

val booleanTerm = bool.map(Lang.Bool(_))

val strTerm = str.map(Lang.Str(_))

val primitiveTypes = oneOf(List(floatTerm.backtrack | integerTerm, booleanTerm, strTerm))

val smallTerm = oneOf(List(primitiveTypes, listTerm, recordTerm, symbolTerm, typeTerm))

@main def testa() =

  val strings = Vector(
    """z = ()""",
    """ ( x = 1 , y = 2,  z = ()) """,
    "z",
    "z z",
    """foo ( 3, 4 , 5 ) (a = 2, b = 3)""",
    "x a ; z z ; z z"
  )

  println(parenElements.parse("x = 1 , y  = 2 , z = ()  "))
  println(parenElements.parse("x"))
  println(parenElements.parse("x, "))
  println("-" * 20)

  val s1 = "(1, 2, 3)"

  Lang.rec(x = Lang.Unit, y = Lang.Integer(2))
  for string <- strings do println(source.parse(string))
