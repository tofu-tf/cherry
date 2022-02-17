package cherry.lamr

import org.scalatest.funsuite.AnyFunSuite
import cherry.utils.TofuDisplay
import cats.syntax.show.given
import scala.deriving.Mirror
import scala.CanEqual.derived
import tofu.common.Display

class TestTest extends munit.FunSuite {
  test("lol") {
    assertEquals("kek", "kek")
  }
}

case class A(s: String) derives TofuDisplay

case class B(x: String, y: List[Int] = Nil, as: List[A] = Nil, b: Option[B] = None) derives TofuDisplay
