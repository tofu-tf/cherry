package cherry.fix

import cats.Functor
import tofu.syntax.monadic.given
import cats.Traverse
import cats.Eval
import tofu.syntax.collections.given

type Fix[+G[+_]] = Fix.T[G]

object Fix:
  opaque type T[+G[+_]] >: G[Nothing] <: G[Any] = G[Any]

  def apply[G[+_]](gf: G[T[G]]): T[G] = gf

  extension [G[+_]](tg: T[G])
    def unpack: G[T[G]] = tg.asInstanceOf[G[T[G]]]

    def fold[R](f: G[R] => R)(using Functor[G]): R =
      f(tg.unpack.map(_.fold(f)))

    def foldDefer[R](f: G[Eval[R]] => Eval[R])(using Traverse[G]): Eval[R] =
      Eval.defer(f(tg.unpack.map(_.foldDefer(f))))

    def folds[R](f: G[R] => R)(using Traverse[G]): R =
      tg.foldDefer[R](_.sequence.map(f)).value

    def foldHist[R](f: G[Cofree[G, R]] => Eval[R])(using Traverse[G]): Cofree[G, R] = {
      val gc = Eval.later(tg.unpack.map(_.foldHist(f)))
      Cofree(gc.flatMap(f).memoize, gc)
    }
