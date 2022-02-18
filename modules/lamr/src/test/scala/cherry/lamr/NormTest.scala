package cherry.lamr

import cherry.lamr.norm.umami.{RecordValue, UmamiNormalizer, UnitValue}
import cherry.lamr.norm.{BuiltinLibrary, Cause, Error, NormValue, Normalizer, Term, State}
import cherry.fix.Fix
import cherry.lamr.norm.ints.IntsLibrary
import munit.Clue

class NormTest extends munit.FunSuite:
  def debugNorm(t: Term, ctx: NormValue, state: State) = println(s"normalizing $t")

  val normalizer: Normalizer = UmamiNormalizer(BuiltinLibrary)

  def errClues(err: Error): IArray[Clue[_]] = IArray(err.cause, err.value)

  extension (t: Fix[Lang])
    infix def shouldNorm(to: Fix[Lang]) =
      val state   = State()
      val res     = normalizer.normalize(t, BuiltinLibrary).map(_.view(BuiltinLibrary)).run(state, maxSteps = 1000)
      val message = "errors during calculation"
      assert(res.isDefined, clue = clues((message +: state.errors.flatMap(errClues))*))
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

  val intPlus = Lang.get.ints |> Lang.get.plus

  test("plus application") {
    val sum = intPlus(Lang.rec(Lang.Integer(1), Lang.Integer(2)))

    sum shouldNorm Lang.Integer(3)
  }

  val plusOne = Lang.Capture(Lang.recT(x = intType), intPlus(Lang.rec(Lang.get.x, Lang.Integer(1)))).fix

  test("int lambda application") {
    plusOne.call(x = Lang.Integer(4)) shouldNorm Lang.Integer(5)
  }

  val plusCurried = Lang
    .Capture(
      Lang.recT(x = intType),
      Lang.Capture(Lang.recT(y = intType), intPlus(Lang.rec(Lang.get.x, Lang.get.y))).fix
    )
    .fix

  test("curried lambda application") {
    plusCurried.call(x = Lang.Integer(6)).call(y = Lang.Integer(-4)) shouldNorm Lang.Integer(2)
  }
