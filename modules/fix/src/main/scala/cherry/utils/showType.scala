package cherry.utils
import scala.quoted.*

inline def showType[T <: AnyKind]: String = ${ showTypeMacro[T] }

def showTypeMacro[T <: AnyKind: Type](using q: Quotes): Expr[String] =
  import q.reflect.*

  val t = TypeRepr.of[T].dealias.widen

  Expr(t.show)

inline def infoType[T <: AnyKind]: Unit                              = ${ infoTypeMacro[T] }

def infoTypeMacro[T <: AnyKind: Type](using q: Quotes): Expr[Unit] =
  import q.reflect.*
  
  val t = TypeRepr.of[T]

  report.info(TypeRepr.of[T].dealias.widen.show)

  '{ () }

inline def debug(inline s: String): Unit                           = ${ debugMacro('s) }

def debugMacro(s: Expr[String])(using q: Quotes): Expr[Unit] =

  import q.reflect.*

  report.info(s.value.getOrElse("???"))

  '{ () }
