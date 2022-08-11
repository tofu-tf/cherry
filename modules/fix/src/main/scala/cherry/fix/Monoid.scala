package cherry.fix

trait Semigroup[A]:
  def add(x: A, y: A): A

  extension (x: A) def ++(y: A) = add(x, y)

object Semigroup:
  given Monoid[String] with
    def zero                      = ""
    def add(x: String, y: String) = x + y

  given Monoid[Int] with
    def zero = 0

    def add(x: Int, y: Int) = x + y

trait Monoid[A] extends Semigroup[A]:
  def zero: A

object Monoid:
  def zero[A](using A: Monoid[A]) = A.zero
