package cherry.lamr.parse

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import cherry.lamr.Lang
import cherry.fix.Fix
import org.scalactic.source.Position
import org.scalactic.Prettifier
import org.scalactic.Equality
import cherry.lamr.parse.term.source

class TermSuite extends AnyFunSuite with Matchers:
  extension (exp: String)(using Position, Prettifier)
    infix def shouldParse(lang: Fix[Lang]) =
      source.parse(exp) shouldEqual Right(("", lang))

  test("unit") {
    "[]" shouldParse Lang.Unit
  }

  test("singleton tuple") {
    "[1]" shouldParse Lang.rec(1)
  }

  test("integer tuple") {
    "[1, 2 ,3]" shouldParse Lang.rec(1, 2, 3)
  }

  test("record") {
    "(a = 1, b = 2, c = 3)" shouldParse Lang.rec(a = 1, b = 2, c = 3)
  }

  test("sample application") {
    "x z" shouldParse Lang.get("x").apply(Lang.get("z"))
  }

  test("complex application") {
    "x [a, b]" shouldParse Lang.get.x.apply(Lang.rec(Lang.get.a, Lang.get.b))
  }

end TermSuite

