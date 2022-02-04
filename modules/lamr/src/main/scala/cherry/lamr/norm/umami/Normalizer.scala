package cherry.lamr.norm
package umami

import cherry.utils.Act
import cherry.lamr.Lang
import cats.syntax.parallel.given
import tofu.syntax.monadic.given
class UmamiNormalizer(library: Library) extends Normalizer:
  def bigStep(term: PartialTerm, context: NormValue): Process[NormValue] = term.unpack match
    case Symbol(id, key) => Act.pure(Abstract(Symbol(id, key)))

    case Lang.Unit             => Act.pure(UnitValue)
    case Lang.Integer(i)       => Act.pure(IntegerValue(i))
    case Lang.Str(s)           => Act.pure(StringValue(s))
    case Lang.Float(f)         => Act.pure(FloatValue(f))
    case Lang.Bool(b)          => Act.pure(BooleanValue(b))
    case Lang.Type(opts)       => Act.pure(TypeValue(opts))
    case Lang.Id               => Act.pure(context)
    case Lang.Set(k, t)        => for tnorm <- bigStep(t, context) yield RecordValue.single(k, tnorm)
    case Lang.Record(k, t)     =>
      for
        tnorm <- bigStep(t, context)
        ttype <- tnorm.asType
      yield RecordType.single(k, ttype)
    case Lang.Get(k, up)       => context.get(k, up)
    case Lang.Merge(base, ext) =>
      for
        baseNorm <- bigStep(term, context)
        extCtx   <- context.merge(baseNorm)
        extNorm  <- bigStep(ext, extCtx)
        result   <- baseNorm.merge(extNorm)
      yield result

    case Lang.AndThen(tl, tr) =>
      for
        left  <- bigStep(tl, context)
        right <- bigStep(tr, left)
      yield right
      
    case Lang.Extend(tb, te)  =>
      for
        baseNorm <- bigStep(tb, context)
        baseType <- baseNorm.asType
        absBase  <- baseType.asAbstract
        extCtx   <- context.merge(absBase)
        extNorm  <- bigStep(te, extCtx)
        extType  <- extNorm.asType
      yield ExtendType(baseType, extType)

    case Lang.Narrow(t, domain) =>
      (
        bigStep(t, context),
        bigStep(domain, context)
      ).parMapN(_.narrow(_)).flatten
