package cherry.fix

import cats.Functor
import tofu.syntax.monadic.given
import cats.Traverse
import cats.Eval
import tofu.syntax.collections.given

type Fix[+G[+_]] = Fix.Fix[G]

object Fix:
  opaque type Fix[+G[+_]] >: G[Nothing] <: G[Any] = G[Any]

  def apply[G[+_]](gf: G[Fix[G]]): Fix[G] = gf

  extension [G[+_]](fix: Fix[G])
    def unpack: G[Fix[G]] = fix.asInstanceOf[G[Fix[G]]]

    def fold[R](f: G[R] => R)(using Functor[G]): R =
      f(fix.unpack.map(_.fold(f)))

    def foldDefer[R](f: G[Eval[R]] => Eval[R])(using Traverse[G]): Eval[R] =
      Eval.defer(f(fix.unpack.map(_.foldDefer(f))))

    def folds[R](f: G[R] => R)(using Traverse[G]): R =
      fix.foldDefer[R](_.sequence.map(f)).value

    def foldHist[R](f: G[Cofree[G, R]] => Eval[R])(using Traverse[G]): Cofree[G, R] =
      val gc = Eval.later(fix.unpack.map(_.foldHist(f)))
      Cofree(gc.flatMap(f).memoize, gc)
    end foldHist
end Fix