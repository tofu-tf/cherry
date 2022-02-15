package cherry.lamr

import cherry.lamr.norm.umami.{RecordValue, UmamiNormalizer, UnitValue}
import cherry.lamr.norm.{BuiltinLibrary, Cause, NormValue, Normalizer, State}
import cherry.fix.Fix
import cherry.lamr.norm.PartialTerm
import cherry.lamr.norm.ints.IntsLibrary
import munit.Clue

class NormTest extends munit.FunSuite:
  def debugNorm(t: PartialTerm, ctx: NormValue, state: State) = println(s"normalizing $t")

  val normalizer: Normalizer = UmamiNormalizer(BuiltinLibrary)

  extension (t: Fix[Lang])
    infix def shouldNorm(to: Fix[Lang]) =
      val state = State()
      val init  = RecordValue.from(RecordKey.Symbol("ints") -> IntsLibrary)
      val res   = normalizer.normalize(t, init).map(_.toPartial).run(state, maxSteps = 1000)
      assert(res.isDefined, clue = clues("errors during calculation" +: state.errors.map(_.cause: Clue[Cause])*))
      assertEquals(res.get, to)

  test("int norm is same") {
    Lang.Integer(13) shouldNorm Lang.Integer(13)
  }

  test("float norm is same") {
    Lang.Float(-1.23) shouldNorm Lang.Float(-1.23)
  }

  val intType  = Lang.Builtin(BuiltinType.Integer)
  val constLam = Lang.Capture(intType, Lang.Integer(1)).fix

  test("lambda") {
    constLam shouldNorm constLam
  }

  test("lambda application") {
    constLam(Lang.Integer(2)) shouldNorm Lang.Integer(1)
  }

  test("plus application") {
    val sum = (Lang.get("ints") |> Lang.get("plus"))(Lang.rec(Lang.Integer(1), Lang.Integer(2)))

    sum shouldNorm Lang.Integer(3)
  }
