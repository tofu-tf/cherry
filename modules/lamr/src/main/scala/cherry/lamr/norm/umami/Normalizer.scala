package cherry.lamr.norm
package umami

import cherry.lamr.{Lang, LibRef, TypeOptions}
import cherry.utils.{Act, ActMethods, Raising}

class UmamiNormalizer(library: Library, dbg: (Term, cherry.lamr.norm.NormValue, State) => Unit = (_, _, _) => ())
    extends Normalizer:

  def normalize(term: Term): Process[NormValue]                               =
    Process.context.flatMap(context => Process.action(ctx => dbg(term, context, ctx.state))) >> bigStep(term)

  private def bigTypeStep(term: Term): Process[NormType]                      =
    normalize(term).flatMap(_.asType)

  private def bigStepOnce(lang: Lang[Process[NormValue]]): Process[NormValue] = lang match
    case Lang.External(ref)  => library.resolve(ref, this)
    case Lang.Universe(opts) => Process.pure(UniverseType(opts))
    case Lang.Builtin(bt)    => Process.pure(BuiltinNormType(bt))

    case Lang.Extend(baseProc, extProc) =>
      for
        baseNorm <- baseProc
        baseType <- baseNorm.asType
        absBase  <- baseType.asAbstract
        extNorm  <- extProc.locallyMergeContext(absBase)
        extType  <- extNorm.asType
      yield ExtendType(baseType, extType)

    case Lang.Function(domain, effect, body) =>
      for
        domType  <- domain >>= (_.asType)
        domAbs  <- domType.asAbstract
        bodyType <- (body >>= (_.asType)).locallyMergeContext(domAbs)
        effType  <- effect >>= (_.asType)
      yield FunctionType(domType, effType, bodyType)

    case Lang.Merge(base, ext) =>
      for
        baseNorm <- base
        extNorm  <- ext.locallyMergeContext(baseNorm)
        result   <- baseNorm.merge(extNorm)
      yield result

    case Lang.Capture(domain, body) =>
      for
        domType <- domain >>= (_.asType)
        context <- Process.context
      yield Closure(context, body, domType)

  private def bigStep(term: Term): Process[NormValue]                         = term.unpack match

    case Lang.External(ref) => library.resolve(ref, this)

    case Lang.Universe(opts) => Act.pure(UniverseType(opts))

    case Lang.Builtin(bt) => Act.pure(BuiltinNormType(bt))

    case Lang.Extend(tb, te) =>
      for
        baseNorm <- normalize(tb)
        baseType <- baseNorm.asType
        absBase  <- baseType.asAbstract
        extNorm  <- normalize(te).locallyMergeContext(absBase)
        extType  <- extNorm.asType
      yield ExtendType(baseType, extType)

    case Lang.Function(domain, effect, body) =>
      for
        domType  <- bigTypeStep(domain)
        domTerm  <- domType.asAbstract
        bodyType <- bigTypeStep(body).locallyMergeContext(domTerm)
        effType  <- bigTypeStep(effect)
      yield FunctionType(domType, effType, bodyType)

    case Lang.Merge(base, ext) =>
      for
        baseNorm <- normalize(base)
        extNorm  <- normalize(ext).locallyMergeContext(baseNorm)
        result   <- baseNorm.merge(extNorm)
      yield result

    case Lang.Capture(domain, body) =>
      for
        domType <- bigTypeStep(domain)
        context <- Process.context
      yield Closure(context, normalize(body), domType)

    case Lang.Apply =>
      Process.context.flatMap(context => context.first.flatMap2Par(context.second)(_.apply(_)))

    case Lang.Record(k, t, _) =>
      for
        tnorm <- normalize(t)
        ttype <- tnorm.asType
      yield RecordType.single(k, ttype)

    case Lang.Unit       => Act.pure(UnitValue)
    case Lang.Integer(i) => Act.pure(IntegerValue(i))
    case Lang.Str(s)     => Act.pure(StringValue(s))
    case Lang.Float(f)   => Act.pure(FloatValue(f))
    case Lang.Bool(b)    => Act.pure(BooleanValue(b))

    case Lang.Id => Process.context

    case Lang.Set(k, t) => for tnorm <- normalize(t) yield RecordValue.single(k, tnorm)

    case Lang.GetKey(k, up) => Process.context.flatMap(_.get(k, up))

    case Lang.AndThen(tl, tr) =>
      for
        left  <- normalize(tl)
        right <- normalize(tr).locally(_.context = left)
      yield right

    case Lang.Narrow(t, domain) =>
      normalize(t).flatMap2Par(normalize(domain).flatMap(_.asType))(_.narrow(_))
