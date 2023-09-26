package confuse.parsers

import confuse.model.*
import confuse.util.TextUtils

trait ParserApi:
  trait Parser[A]:
    /** Parse a config value into its scala type.
      * @param value The value to parse
      * @param path The configuration path of the value (for error messages only)
      */
    def parse(value: Value, path: Path): Result[A]
  object Parser

trait StandardParsers extends ParserApi with clam.core.StandardStringReaders:

  given primitiveParser[A](using sr: StringReader[A]): Parser[A] with
    def parse(cfg: Value, path: Path): Result[A] = cfg match
      case Str(s) =>
        sr.read(s) match
          case StringReader.Result.Success(a) => Result.Success(a)
          case StringReader.Result.Error(m) =>
            Result.singleError(FieldError.TypeMismatch(path, cfg, sr.typeName))
      // case n: Null if n.origins.isEmpty =>
      //   Result.singleError(FieldError.Missing(path, sr.typeName))
      case n: Null =>
        Result.singleError(FieldError.TypeMismatch(path, n, sr.typeName))
      case _ =>
        Result.singleError(FieldError.TypeMismatch(path, cfg, sr.typeName))

    given valueParser: Parser[Value] with
      def parse(value: Value, path: Path): Result[Value] = Result.Success(value)

    given cfgParser: Parser[Config] with
      def parse(value: Value, path: Path): Result[Config] = value match
        case c: Config => Result.Success(c)
        case other => Result.singleError(FieldError.TypeMismatch(path, value, "config object"))

    given arrParser: Parser[Arr] with
      def parse(value: Value, path: Path): Result[Arr] = value match
        case c: Arr => Result.Success(c)
        case other => Result.singleError(FieldError.TypeMismatch(path, value, "config array"))

    given strParser: Parser[Str] with
      def parse(value: Value, path: Path): Result[Str] = value match
        case c: Str => Result.Success(c)
        case other => Result.singleError(FieldError.TypeMismatch(path, value, "config string"))

  given mapReader[K, V, M[K, V] <: Iterable[(K, V)]](
    using kr: Parser[K],
    vr: Parser[V],
    factory: collection.Factory[(K, V), M[K, V]]
  ): Parser[M[K, V]] = (v, p) => v match
    case Config(fields) =>
      val errors = collection.mutable.ArrayBuffer.empty[FieldError]
      val items = factory.newBuilder
      for (k, v) <- fields do
        kr.parse(Str(k), p) match
          case Result.Success(parsedKey) =>
            vr.parse(v, p / k) match
              case Result.Success(parsedValue) =>
                items += parsedKey -> parsedValue
              case Result.FieldErrors(errs) =>
                errors ++= errs
          case Result.FieldErrors(errs) => errors ++= errs
      if errors.isEmpty then Result.Success(items.result())
      else Result.FieldErrors(errors.toSeq)
    case Null() => Result.Success(factory.newBuilder.result())
    case _ => Result.singleError(FieldError.TypeMismatch(p, v, "a config map"))

  given colReader[Elem, Col[Elem] <: Iterable[Elem]](
    using elementReader: Parser[Elem],
    factory: collection.Factory[Elem, Col[Elem]]
  ): Parser[Col[Elem]] = (v, p) => v match
    case Arr(elems) =>
      val errors = collection.mutable.ArrayBuffer.empty[FieldError]
      val items = factory.newBuilder
      for (elem, idx) <- elems.zipWithIndex do
        elementReader.parse(elem, p / idx.toString) match
          case Result.Success(item) => items += item
          case Result.FieldErrors(errs) => errors ++= errs
      if errors.isEmpty then Result.Success(items.result())
      else Result.FieldErrors(errors.toSeq)
    case _ => Result.singleError(FieldError.TypeMismatch(p, v, "an array"))

trait DerivationApi extends ParserApi:
  api =>
  def scalaNameToConfigName(str: String) = TextUtils.snakify(str)

  case class ProductParser[A](
    fieldNames: Seq[String],
    fieldDefaults: Seq[Option[() => ?]],
    fieldParsers: Seq[Parser[?]],
    instantiate: Seq[?] => A
  ) extends Parser[A]:
    import collection.mutable as m

    def parse(value: Value, path: Path): Result[A] = value match
      case cfg: Config => parseImpl(cfg, path)
      case cfg: Null => parseImpl(Config(), path)
      case _ => Result.singleError(FieldError.TypeMismatch(path, value, "config object"))

    def parseImpl(cfg: Config, path: Path): Result[A] =
      val fields = new Array[Any](fieldNames.length)
      val errors = m.ArrayBuffer.empty[FieldError]
      var i = 0
      while i < fields.length do
        val segment = scalaNameToConfigName(fieldNames(i))
        cfg.fields.get(segment) match
          case None if fieldDefaults(i).isDefined =>
            fields(i) = fieldDefaults(i).get()
          case None =>
            fieldParsers(i).parse(Null(), path / segment) match
              case Result.Success(a) => fields(i) = a
              case Result.FieldErrors(errs) => errors ++= errs
          case Some(value) =>
            fieldParsers(i).parse(value, path/ segment) match
              case Result.Success(a) => fields(i) = a
              case Result.FieldErrors(errs) => errors ++= errs
        i += 1

      if errors.isEmpty then
        try
          Result.Success(instantiate(fields.toIndexedSeq))
        catch
          case ex: IllegalArgumentException =>
            Result.singleError(
              FieldError.Validation(path, cfg, ex)
            )
      else
        Result.FieldErrors(errors.toSeq)

  // implicit class Derivable(p: Parser.type)
  extension (p: Parser.type)
  // object ProductParser:
    inline def derived[A]: ProductParser[A] = derivedImpl[A].asInstanceOf[ProductParser[A]]
    inline def derivedImpl[A] = ${macros.derivedImpl[api.type, A]('api)}


object macros:
  import scala.quoted.Expr
  import scala.quoted.Quotes
  import scala.quoted.Type


  private def getDefaultParams(using qctx: Quotes)
    (method: qctx.reflect.Symbol): Map[qctx.reflect.Symbol, Expr[_]] =
    import qctx.reflect.*
    val pairs = for
      (param, idx) <- method.paramSymss.flatten.zipWithIndex
      if (param.flags.is(Flags.HasDefault))
    yield
      val term = if method.isClassConstructor then
        val defaultName = s"$$lessinit$$greater$$default$$${idx + 1}"
        Ref(method.owner.companionModule.methodMember(defaultName).head)
      else
        val defaultName = s"${method.name}$$default$$${idx + 1}"
        Ref(method.owner.methodMember(defaultName).head)
      param -> term.asExpr
    pairs.toMap

  def derivedImpl[Api <: DerivationApi: Type, A: Type](api: Expr[Api])(using qctx: Quotes): Expr[DerivationApi#Parser[A]] =
    import qctx.reflect.*

    val tpe = TypeRepr.of[A]
    if !(tpe <:< TypeRepr.of[Product]) then
      report.error(s"${tpe.show} is not a product type")
      return '{???}

    val constructor = tpe.typeSymbol.primaryConstructor //companionModule.methodMember("apply").head

    val fieldNames: List[List[Expr[String]]] =
      for params <- constructor.paramSymss yield
        for param <- params yield
          Expr(param.name)

    val defaults = getDefaultParams(constructor)
    val fieldDefaults: List[List[Expr[Option[() => ?]]]] =
      for params <- constructor.paramSymss yield
        for param <- params yield
          defaults.get(param) match
            case None => '{None}
            case Some(expr) => '{Some(() => ${expr})}

    val fieldParsers: List[List[Expr[DerivationApi#Parser[?]]]] =
      for params <- constructor.paramSymss yield
        for param <- params yield
          val readerType = TypeSelect(
            api.asTerm,
            "Parser"
          ).tpe.appliedTo(param.termRef.widenTermRefByName)
          Implicits.search(readerType) match
            case iss: ImplicitSearchSuccess => iss.tree.asExprOf[DerivationApi#Parser[?]]
            case _ =>
              report.error(s"No ${api.asTerm.tpe.widenTermRefByName.show}.Parser[${param.termRef.widenTermRefByName.show}] available for parameter ${param.name}.", param.pos.get)
              '{???}
        end for
      end for

    val instantiate = '{
      (argss: Seq[?]) =>
        ${
          val base: Term = Select(New(TypeTree.ref(constructor.owner)), constructor)
          var i = 0
          val accesses: List[List[Term]] =
            for params <- constructor.paramSymss yield
              for param <- params yield

                param.termRef.widenTermRefByName.asType match
                  case '[t] =>
                    val expr = '{
                      argss(${Expr(i)}).asInstanceOf[t]
                    }
                    i += 1
                    expr.asTerm
          val application = accesses.foldLeft(base)((lhs, args) => Apply(lhs, args))
          application.asExprOf[A]
        }
    }

    '{
      val p = $api
      val names: Seq[String] = ${Expr.ofSeq(fieldNames.flatten)}
      val defaults: Seq[Option[() => ?]] = ${Expr.ofSeq(fieldDefaults.flatten)}
      val parsers: Seq[DerivationApi#Parser[?]] = ${Expr.ofSeq(fieldParsers.flatten)}
      p.ProductParser(
        names,
        defaults,
        parsers.asInstanceOf[Seq[p.Parser[?]]],
        ${instantiate}
      )
    }
