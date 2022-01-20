package cherry.lamr

import org.scalatest.funsuite.AnyFunSuite
import cherry.utils.TofuDisplay
import cats.syntax.show.given
import scala.deriving.Mirror
import scala.CanEqual.derived
import tofu.common.Display

class TestTest extends AnyFunSuite {
  test("lol") {
    assert("kek" === "kek")
  }
}

case class A(s: String) derives TofuDisplay

case class B(x: String, y: List[Int], as: List[A]) derives TofuDisplay



@main def testa() =
  println("ohaio3")
  import tofu.common.Display.given

  println(B("x", List(1, 2), List(A("1"), A("2"))).display(Display.Config.default.copy(indent = "==")))
