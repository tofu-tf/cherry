package cherry.lamr.norm

import tofu.data.{CalcT, CalcM}
import cats.Parallel
import cats.arrow.FunctionK
import cats.Applicative
import cats.syntax.applicative
//import cherry.lamr.norm.PartialTerm

case class Symbol(id: Long, name: String)

enum Sign:
  case Le, Ge, Eq

case class Inequasion[T](
    symbol: Symbol,
    sign: Sign,
    term: T
)

case class State(
    symbolCount: Long,
)

case class Position(start: Long, end: Long)

case class Error(
    message: String,
   term: Option[cherry.lamr.norm.PartialTerm],
    position: Option[Position],
):
  def raise: Process[Nothing] = CalcM.raise(Vector(this))


/**
 */
trait Par[+A]:
  type X
  type Y
  def x: X
  def y: Y
  def map2(x: X, y: Y): A


type Process[+A] = CalcT[Vector, Any, State, State, Vector[Error], A]

def newSymbol(name: String): Process[Symbol] =
  CalcM.state { state =>
    val count  = state.symbolCount + 1
    val symbol = Symbol(count, name)
    (state.copy(symbolCount = count), symbol)
  }
  

given Parallel[Process] with FunctionK[Process, Process] with
  type F[A] = Process[A]

  def apply[A](x: Process[A]) = x

  def parallel   = this
  def sequential = this

  val monad = summon

  given applicative: Applicative[Process] with
    def pure[A](a: A) = CalcM.pure(a)

    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = ???

  end applicative
