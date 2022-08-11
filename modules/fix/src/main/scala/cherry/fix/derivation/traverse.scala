package cherry.fix
package derivation
package traverse

import scala.deriving.Mirror
import compiletime.{erasedValue, summonFrom, summonInline}
import cherry.fix.Traverse
import cherry.utils.{debug, infoType, showType}

import scala.quoted.{Expr, Quotes, Type}

class SomeType{
  type T
}

inline def deriveTraverse[T[+x]]: Traverse[T]                                                                    =
  val x = new SomeType
  val y = new SomeType

  summonFrom {
    case m: Mirror.ProductOf[T[x.T]] =>
      new:
        def trav[A, B, F[+_]: Monoidal](ta: T[A])(f: A => F[B]): F[T[B]] =
            val tx = ta.asInstanceOf[T[x.T]]
            val elemsA = tupleFromProduct(tx)(using m)(using summonInline)
            val elemsB = traverseTuple[m.MirroredElemTypes, F, x.T, B](elemsA)(f.asInstanceOf[x.T => F[B]])
            val mb = summonInline[Mirror.ProductOf[T[B]]]
            elemsB.map(mb.fromProduct)

    case mx: Mirror.SumOf[T[x.T]] =>
      summonFrom {
        case my: Mirror.SumOf[T[y.T]] =>
          new:
            type TXY = Tuple.Zip[mx.MirroredElemTypes, my.MirroredElemTypes]
            infoType[mx.MirroredElemTypes]
            infoType[my.MirroredElemTypes]
            val instances = collectInstances[mx.MirroredElemTypes, my.MirroredElemTypes, x.T, y.T]

            def trav[A, B, F[+_] : Monoidal](ta: T[A])(f: A => F[B]): F[T[B]] =
              val ma: Mirror.SumOf[T[A]] = summonInline
              val i = ma.ordinal(ta)
              val instance = instances.productElement(i).asInstanceOf[Traverse[T]]
              instance.trav(ta)(f)
      }


  }

type Replace[T <: Tuple, A, B, F[+_]] <: F[Tuple] = T match
  case A *: rest    =>
    Replace[rest, A, B, F] match
      case F[t] => F[B *: t]
  case head *: rest =>
    Replace[rest, A, B, F] match
      case F[t] => F[head *: t]
  case EmptyTuple   => F[EmptyTuple]

inline def traverseTuple[T <: Tuple, F[+_], A, B](t: T)(f: A => F[B])(using F: Monoidal[F]): F[Tuple] =
  infoType[T]
  inline t match
    case c: (head *: rest)    =>
      infoType[head]
      type Head = head
      summonFrom{
        case eqt: (Head =:= A) =>
          f(c.head).map2(traverseTuple[rest, F, A, B](c.tail)(f))(_ *: _)
        case _ =>
          traverseTuple[rest, F, A, B](c.tail)(f).map(c.head *: _)
      }
    case _: EmptyTuple     =>
      F.emptyTuple

inline def collectInstances[TX <: Tuple, TY <: Tuple, X, Y]: Tuple                                                           =
  inline erasedValue[TX] match
    case _: (fx *: restX) =>
      inline erasedValue[TY] match {
        case _ : (fy *: restY) =>
          type FX = fx
          type FY = fy
          debug("collect instances: " + showType[FX] + ": " + showType[X]  + " , "+ showType[FY]+ ": " + showType[Y])

          summonFrom {
            case un: Unapply[FX, X] =>
              type TF = Traverse[un.T]
              val tf: TF = summonFrom {
                case tf: TF => tf
                case _ => deriveTraverse[un.T]
              }
              tf *: collectInstances[restX, restY, X, Y]
            case given (FX =:= FY) =>
              constTraverse[fx] *: collectInstances[restX, restY, X, Y]
          }
      }

    case EmptyTuple            => EmptyTuple

def constTraverse[X]: Traverse[[a] =>> X] = new:
  def trav[A, B, F[+_]: Monoidal](x: X)(f: A => F[B]): F[X] = Monad.pure[F](x)

class Unapply[FX, X]:
  type T[+x]

object Unapply:
  given [F[+_], X]: Unapply[F[X], X] with
    type T[x] = F[x]
