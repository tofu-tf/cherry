package cherry.utils

import tofu.common.Display
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, summonFrom, constValueTuple, constValue}
import cats.Eval
import tofu.syntax.collections.*
import tofu.optics.tags.field
import scala.quoted.{Quotes, Type}
import scala.quoted.Expr

trait DisplayK[F[_]]:
  def displayWith[A: Display]: Display[F[A]]

trait TofuDisplay[A] extends Display[A]:
  def displayBuildIn(
      cfg: Display.Config,
      a: A,
      indent: Vector[String] = Vector.empty,
      label: Vector[String] = Vector.empty
  ): Eval[Vector[String]]

  def displayBuild(cfg: Display.Config, a: A): Eval[Vector[String]] = displayBuildIn(cfg, a)

object TofuDisplay:

  class FromDisplay[A](d: Display[A]) extends TofuDisplay[A]:
    def displayBuildIn(cfg: Display.Config, a: A, indent: Vector[String], label: Vector[String]): Eval[Vector[String]] =
      d.displayBuild(cfg, a).map(label.++)

    override def displayBuild(cfg: Display.Config, a: A): Eval[Vector[String]] =
      d.displayBuild(cfg, a)

  end FromDisplay

  def tnameMacro[A](using Type[A])(using Quotes): Expr[String] = Expr(Type.show[A])

  inline def tname[A]: String = ${ tnameMacro[A] }

  inline def derived[A](using m: Mirror.Of[A]): TofuDisplay[A] =
    println("gogo")
    val instances: Vector[TofuDisplay[Any]] = summonDisplays[m.MirroredElemTypes].asInstanceOf[Vector[TofuDisplay[Any]]]
    inline m match
      case m: Mirror.SumOf[A]     => derivedSum(m, instances)
      case m: Mirror.ProductOf[A] => derivedProduct(m, instances)

  private inline def derivedSum[A](m: Mirror.SumOf[A], instances: Vector[TofuDisplay[Any]]): TofuDisplay[A] =
    (cfg, a: A, ind, lab) => instances(m.ordinal(a)).displayBuildIn(cfg, a, ind, lab)

  private inline def derivedProduct[A](m: Mirror.ProductOf[A], instances: Vector[TofuDisplay[Any]]): TofuDisplay[A] =
    val names = constValueTuple[m.MirroredElemLabels].toIArray.map(_.asInstanceOf[String])
    val name  = constValue[m.MirroredLabel]
    (cfg, a: A, ind, lab) =>

      println(s"for $a indent is ${ind}")

      val inner             = ind :+ cfg.indent
      val product           = a.asInstanceOf[Product]
      val shortName: String = product.productPrefix
      val start             = ind ++ lab ++ Vector(shortName, cfg.brackets.left, cfg.newline)
      val end               = ind :+ cfg.brackets.right

      def buildStep(fieldResult: Vector[String], i: Int): Vector[String] =
        val adapted       = fieldResult
        val withSeparator = if i + 1 < product.productArity then adapted :+ cfg.fieldSeparator else adapted
        val fullValue     = withSeparator :+ cfg.newline
        fullValue

      product.productIterator.toVector
        .traverseWithIndexM[Eval, Vector[String]] { (el, i) =>
          val label = if cfg.showFieldLabels then Vector(names(i), cfg.fieldAssign) else Vector()
          instances(i).displayBuildIn(cfg, el, inner, label).map { buildStep(_, i) }
        }
        .map(vs => (start +: vs :+ end).flatten)

  end derivedProduct

  private inline def summonDisplays[T <: Tuple]: Vector[TofuDisplay[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Vector.empty
      case _: (t *: ts)  =>
        type T = t
        println(tname[t])
        summonFrom {
          case d: TofuDisplay[T] => d +: summonDisplays[ts]
          case _                 => FromDisplay(summonInline[Display[t]]) +: summonDisplays[ts]
        }
