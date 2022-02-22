package cherry.adapt

import cats.data.NonEmptyVector
import cats.parse.Parser
import cherry.fix.Fix.Fix
import cherry.lamr.{Lang, norm, parse}
import cherry.lamr.norm.{BuiltinLibrary, Library, NormValue, Normalizer, State, Term}

class Expression(val expr: Term):
  def unpack: Lang[Expression] =
    expr.unpack match
      case l: Lang[Term] => l.map(Expression(_))

  def normalize(n: Normalizer, context: NormValue = BuiltinLibrary): Either[Vector[norm.Error], NormValue] =
    val state = State()
    n.normalize(expr, context).run(state).toRight(state.errors)

object Parsing:
  val sourceTerm: Parser[Term] = parse.term.source

  val sourceExpression: Parser[Expression] = sourceTerm.map(Expression(_))
