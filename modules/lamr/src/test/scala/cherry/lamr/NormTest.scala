package cherry.lamr

import cherry.lamr.norm.umami.UmamiNormalizer
import cherry.lamr.norm.BuiltinLibrary
import cherry.lamr.norm.umami.UnitValue
import cherry.lamr.norm.State
import cherry.fix.Fix

class NormTest extends munit.FunSuite:
  val normalizer = UmamiNormalizer(BuiltinLibrary)

  extension (t: Fix[Lang])
    def shouldNorm(to: Fix[Lang]) =
      val res = normalizer.bigStep(t, UnitValue).map(_.toPartial).run(State())
      assertEquals(res, Some(to))

  test("int norm is same") {
    Lang.Integer(13) shouldNorm Lang.Integer(13)
  }

  test("float norm is same"){
    Lang.Float(-1.23) shouldNorm Lang.Float(-1.23)
  }
