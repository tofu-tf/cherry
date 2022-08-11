package cherry.fix

import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec

object Fix:
  opaque type Fix[+G[+_]] >: G[Nothing] <: G[Any] = G[Any]

  given [G[+_]]: Conversion[G[Fix[G]], Fix[G]] = Fix(_)

  def apply[G[+_]](gf: G[Fix[G]]): Fix[G] = gf

  extension [G[+_]](fix: Fix[G])
    inline def unpack: G[Fix[G]] = fix.asInstanceOf[G[Fix[G]]]

    def fold[R](f: G[R] => R)(using Functor[G]): R =
      f(fix.unpack.map(_.fold(f)))

    def foldDefer[R](f: G[TailRec[R]] => TailRec[R])(using Traverse[G]): TailRec[R] =
      tailcall(f(fix.unpack.map(_.foldDefer(f))))

    def folds[R](f: G[R] => R)(using Traverse[G]): R =
      fix.foldDefer[R](_.sequence.map(f)).result

    def foldHist[R](f: G[Cofree[G, R]] => TailRec[R])(using Traverse[G]): Cofree[G, R] =
      val gc = delay(fix.unpack.map(_.foldHist(f)))
      Cofree(gc.flatMap(f).memoize, gc)
    end foldHist
end Fix
