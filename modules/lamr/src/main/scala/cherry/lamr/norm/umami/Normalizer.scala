package cherry.lamr.norm
package umami

import cats.syntax.parallel.given
import cherry.lamr.{Lang, LibRef, TypeOptions}
import cherry.utils.Act
import tofu.syntax.monadic.given
class UmamiNormalizer(library: Library, dbg: (Term, cherry.lamr.norm.NormValue, State) => Unit = (_, _, _) => ())
    extends Normalizer:

  def normalize(term: Term, context: NormValue): Process[NormValue]          =
    Act.action(st => dbg(term, context, st)) >> bigStep(term, context)

  private def bigTypeStep(term: Term, context: NormValue): Process[NormType] =
    normalize(term, context).flatMap(_.asType)

  private def bigStep(term: Term, context: NormValue): Process[NormValue]    = term.unpack match

    case Lang.External(ref) => library.resolve(context, ref, this)

    case Lang.Universe(opts) => Act.pure(UniverseType(opts))

    case Lang.Builtin(bt) => Act.pure(BuiltinNormType(bt))

    case Lang.Extend(tb, te) =>
      for
        baseNorm <- normalize(tb, context)
        baseType <- baseNorm.asType
        absBase  <- baseType.asAbstract
        extCtx   <- context.merge(absBase)
        extNorm  <- normalize(te, extCtx)
        extType  <- extNorm.asType
      yield ExtendType(baseType, extType)

    case Lang.Function(domain, effect, body) =>
      for
        domType  <- bigTypeStep(domain, context)
        domTerm  <- domType.asAbstract
        extCtx   <- context.merge(domTerm)
        bodyType <- bigTypeStep(body, extCtx)
        effType  <- bigTypeStep(effect, context)
      yield FunctionType(domType, effType, bodyType)

    case Lang.Merge(base, ext) =>
      for
        baseNorm <- normalize(base, context)
        extCtx   <- context.merge(baseNorm)
        extNorm  <- normalize(ext, extCtx)
        result   <- baseNorm.merge(extNorm)
      yield result

    case Lang.Capture(domain, body) =>
      for domType <- bigTypeStep(domain, context)
      yield Closure(context, body, domType, this)

    case Lang.Apply =>
      context.first.flatMap2Par(context.second)(_.apply(_))

    case Lang.Record(k, t, _) =>
      for
        tnorm <- normalize(t, context)
        ttype <- tnorm.asType
      yield RecordType.single(k, ttype)

    case Lang.Unit       => Act.pure(UnitValue)
    case Lang.Integer(i) => Act.pure(IntegerValue(i))
    case Lang.Str(s)     => Act.pure(StringValue(s))
    case Lang.Float(f)   => Act.pure(FloatValue(f))
    case Lang.Bool(b)    => Act.pure(BooleanValue(b))

    case Lang.Id => Act.pure(context)

    case Lang.Set(k, t) => for tnorm <- normalize(t, context) yield RecordValue.single(k, tnorm)

    case Lang.GetKey(k, up) => context.get(k, up)

    case Lang.AndThen(tl, tr) =>
      for
        left  <- normalize(tl, context)
        right <- normalize(tr, left)
      yield right

    case Lang.Narrow(t, domain) =>
      normalize(t, context).flatMap2Par(normalize(domain, context).flatMap(_.asType))(_.narrow(_))
