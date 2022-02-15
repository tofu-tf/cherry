package cherry.lamr.norm

enum Sign:
  case Le, Ge, Eq

case class Inequasion[T](
    symbol: Long,
    sign: Sign,
    term: T,
)

trait InequasionSystem[T]:
  def +(ieq: Inequasion[T]): Either[String, InequasionSystem[T]]

class DummyIneqSystem[T] extends InequasionSystem[T]:
  def +(ieq: Inequasion[T]) = Right(this) 
