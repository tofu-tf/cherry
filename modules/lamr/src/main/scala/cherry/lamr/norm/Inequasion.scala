package cherry.lamr.norm

enum Sign:
  case Le, Ge, Eq

case class Inequasion[T](
    symbol: Symbol,
    sign: Sign,
    term: T,
)

trait InequasionSystem[T]:
  def +(ieq: Inequasion[T]): Either[String, InequasionSystem[T]]
