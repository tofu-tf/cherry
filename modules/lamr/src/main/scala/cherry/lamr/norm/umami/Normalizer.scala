package cherry.lamr.norm
package umami

import cherry.utils.Act
import cherry.lamr.Lang
import cats.syntax.parallel.given
import tofu.syntax.monadic.given
import cherry.lamr.LibRef
class UmamiNormalizer(library: Library) extends Normalizer:
  def bigTypeStep(term: PartialTerm, context: NormValue): Process[NormType] =
    bigStep(term, context).flatMap(_.asType)

  def bigStep(term: PartialTerm, context: NormValue): Process[NormValue] = term.unpack match
    case s @ Symbol(id, key, term) =>
      for tpe <- bigTypeStep(term, context)
      yield Abstract(s, tpe)

    case Lang.External(ref) => library.resolve(context, ref, this)

    case Lang.Universe(opts) => Act.pure(UniverseType(opts))

    case Lang.Builtin(bt) => Act.pure(BuiltinNormType(bt))

    case Lang.Extend(tb, te) =>
      for
        baseNorm <- bigStep(tb, context)
        baseType <- baseNorm.asType
        absBase  <- baseType.asAbstract
        extCtx   <- context.merge(absBase)
        extNorm  <- bigStep(te, extCtx)
        extType  <- extNorm.asType
      yield ExtendType(baseType, extType)

    case Lang.Function(domain, body) =>
      for
        domNorm  <- bigStep(domain, context)
        domType  <- domNorm.asType
        domTerm  <- domType.asAbstract
        extCtx   <- context.merge(domTerm)
        bodyNorm <- bigStep(body, extCtx)
        bodyType <- bodyNorm.asType
      yield FunctionType(domType, bodyType)

    case Lang.Merge(base, ext) =>
      for
        baseNorm <- bigStep(term, context)
        extCtx   <- context.merge(baseNorm)
        extNorm  <- bigStep(ext, extCtx)
        result   <- baseNorm.merge(extNorm)
      yield result

    case Lang.Capture(domain, body) =>
      for
        domNorm <- bigStep(domain, context)
        domType <- domNorm.asType
      yield Closure(context, body, domType, this)

    case Lang.Apply =>
      context.first.flatMap2Par(context.second)(_.apply(_))

    case Lang.Record(k, t) =>
      for
        tnorm <- bigStep(t, context)
        ttype <- tnorm.asType
      yield RecordType.single(k, ttype)

    case Lang.Unit       => Act.pure(UnitValue)
    case Lang.Integer(i) => Act.pure(IntegerValue(i))
    case Lang.Str(s)     => Act.pure(StringValue(s))
    case Lang.Float(f)   => Act.pure(FloatValue(f))
    case Lang.Bool(b)    => Act.pure(BooleanValue(b))

    case Lang.Id => Act.pure(context)

    case Lang.Set(k, t) => for tnorm <- bigStep(t, context) yield RecordValue.single(k, tnorm)

    case Lang.Get(k, up) => context.get(k, up)

    case Lang.AndThen(tl, tr) =>
      for
        left  <- bigStep(tl, context)
        right <- bigStep(tr, left)
      yield right

    case Lang.Narrow(t, domain) =>
      bigStep(t, context).flatMap2Par(bigStep(domain, context).flatMap(_.asType))(_.narrow(_))
