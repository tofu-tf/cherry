package cherry.fix.derivation

import scala.deriving.Mirror

def tupleFromProduct[A](a: A)(using m: Mirror.ProductOf[A])(using A <:< Product): m.MirroredElemTypes =
  val coerce = subtypeToIntersectionEq[A, Product]
  type MP[a] = Mirror.ProductOf[a] { type MirroredElemTypes = m.MirroredElemTypes }
  val coeMirror = coerce.liftCo[MP](m)
  Tuple.fromProductTyped[A & Product](coerce(a))(using coeMirror)
end tupleFromProduct

private def subtypeToIntersectionEq[A, B](using ev: A <:< B): A =:= (A & B)                           =
  given (A <:< (A & B)) = ev.liftCo[[x] =>> A & x]
  <:<.antisymm
