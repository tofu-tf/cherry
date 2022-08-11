package cherry.utils

import cherry.fix.{Monoid, Monoidal, Traverse}

import scala.collection.immutable.TreeMap
import scala.collection.immutable.SortedMap
import scala.annotation.tailrec
import scala.collection.mutable
import cherry.fix.Functor.given

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
    extension[A](fa: LayeredMap[K, A])
      def traverse[G[+_], B](f: A => G[B])(using G: Monoidal[G]): G[LayeredMap[K, B]] =
        fa.journal.traverse { case (k, v) => f(v) }.map(v => fromVector(fa.journal.iterator.map(_._1).zip(v).toVector))

@tailrec
def length(c: Char)(x: Int = c.toInt * c.toInt, len: Int = 0): Int =
  if (x == c.toInt) len else length(c)((x * c.toInt) % Char.MaxValue, len + 1)

