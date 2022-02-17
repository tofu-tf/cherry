package cherry.utils

import scala.collection.immutable.TreeMap
import cats.Traverse
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import cats.Eval
import cats.Applicative
import tofu.syntax.collections.given
import tofu.syntax.monadic.given
import scala.collection.mutable

case class LayeredMap[K, +V](values: Map[K, List[V]], journal: Vector[(K, V)]):
  def get(key: K, up: Int): Option[V] = values.get(key).flatMap(_.lift(up))

  def put[V1 >: V](key: K, value: V1) = LayeredMap(
    values.updatedWith(key) {
      case Some(l) => Some(value :: l)
      case None    => Some(List(value))
    },
    journal :+ (key, value)
  )

object LayeredMap:
  def fromVector[K, V](v: Vector[(K, V)]): LayeredMap[K, V] = {
    val mut = new mutable.HashMap[K, List[V]]
    for ((k, v) <- v) mut.updateWith(k) {
      case Some(l) => Some(v :: l)
      case None    => Some(List(v))
    }
    LayeredMap(mut.toMap, v)

  }

  given [K]: Traverse[[a] =>> LayeredMap[K, a]] with
    def foldLeft[A, B](fa: LayeredMap[K, A], b: B)(f: (B, A) => B): B =
      fa.journal.foldLeft(b) { case (b, (_, v)) => f(b, v) }

    def foldRight[A, B](fa: LayeredMap[K, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
      val v                   = fa.journal
      def fr(i: Int): Eval[B] = if i == v.length then lb else Eval.defer(f(v(i)._2, fr(i + 1)))
      fr(0)

    def traverse[G[_], A, B](fa: LayeredMap[K, A])(f: A => G[B])(using G: Applicative[G]): G[LayeredMap[K, B]] =
      fa.journal.traverse { case (k, v) => f(v) }.map(v => fromVector(fa.journal.iterator.map(_._1).zip(v).toVector))

@tailrec
def length(c: Char)(x: Int = c.toInt * c.toInt, len: Int = 0): Int =
  if (x == c.toInt) len else length(c)((x * c.toInt) % Char.MaxValue, len + 1)

@main def hello() =
  for c <- 'a' to 'z' do println(c -> length(c)())

// def kriwda(s: String) =
//   (s.view.map(_.toInt).sum, s.foldLeft(0)(_ ^ _), s.foldLeft(1)((x, y) => (x * y) % Char.MaxValue))
