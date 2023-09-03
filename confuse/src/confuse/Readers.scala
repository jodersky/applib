package confuse

enum FieldError:
  case Mismatch(path: Path, value: Value, message: String)
  case Validation(path: Path, value: Value, message: String)

  def pretty = this match
    case Mismatch(path, value, _) if value.origins == Nil =>
      s"missing required configuration '$path'"
    case Mismatch(path, value, message) =>
      s"type mismatch in configuration '$path': ${FieldError.shortTpe(value)} $message\n${FieldError.origin(value)}"
    case Validation(path, value, message) =>
      s"validation error in configuration '$path': $message\n${FieldError.origin(value)}"

object FieldError:
  private def shortTpe(value: Value) = value match
    case Null() => "<null>"
    case Str(s) =>
      val max = 10
      if s.length > max then s"'${s.take(max)}...'" else s"'$s'"
    case _: Arr => "<config array>"
    case _: Config => "<config object>"

  private def origin(value: Value): String = value.origins match
    case Nil => ""
    case head :: _ =>
      s"  at ${head.pretty}"

enum Result[+A]:
  case Success(a: A)
  case Error(errors: Seq[FieldError]) extends Result[Nothing]

  def map[B](fn: A => B): Result[B] = this match
    case Success(a) => Success(fn(a))
    case e: Error => e

  def flatMap[B](fn: A => Result[B]): Result[B] = this match
    case Success(a) => fn(a)
    case e: Error => e

object Result:
  def single(err: FieldError) = Result.Error(Seq(err))


trait ReaderApi:
  api =>

  trait Reader[A]:
    def read(value: Value, path: List[String]): Result[A]

  object Reader:
    inline def derived[A]: Reader[A] = derivedImpl[A].asInstanceOf[Reader[A]]
    private inline def derivedImpl[A] = ${
      DerivationMacros.derivedImpl[api.type, A]('api)
    }

trait StandardReaders extends ReaderApi with clam.core.StandardStringReaders:

  given Reader[Value] = (v, p) => Result.Success(v)

  given Reader[Config] = (v, p) => v match
    case c: Config => Result.Success(c)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected a config object"))

  given Reader[Arr] = (v, p) => v match
    case a: Arr => Result.Success(a)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected a config array"))

  given Reader[Str] = (v, p) => v match
    case s: Str => Result.Success(s)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected a string"))

  given Reader[Null] = (v, p) => v match
    case n: Null => Result.Success(n)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected an unset field"))

  given [A](using sr: StringReader[A]): Reader[A] = (v, p) => v match
    case Str(s) =>
      sr.read(s) match
        case StringReader.Result.Success(a) => Result.Success(a)
        case StringReader.Result.Error(m) => Result.single(FieldError.Mismatch(p, v, m))
    case _ => Result.single(FieldError.Mismatch(p, v, "expected a string"))


  // given Reader[String] = (v, p) => v match
  //   case s: Str => Result.Success(s.str)
  //   case _ => Result.single(FieldError.Mismatch(p, v, "a string"))

  // given [N](using n: Integral[N]): Reader[N] = (v, p) => v match
  //   case Str(s) =>
  //     try
  //       Result.Success(n.fromInt(s.toInt))
  //     catch
  //       case _: NumberFormatException =>
  //         Result.single(FieldError.Mismatch(p, v, "an integral number"))
  //   case _ => Result.single(FieldError.Mismatch(p, v, "an integral number"))

  // given Reader[Float] = (v, p) => v match
  //   case Str(s) =>
  //     try
  //       Result.Success(s.toFloat)
  //     catch
  //       case _: NumberFormatException =>
  //         Result.single(FieldError.Mismatch(p, v, "a floating point number"))
  //   case _ => Result.single(FieldError.Mismatch(p, v, "a floating point number"))

  // given Reader[Double] = (v, p) => v match
  //   case Str(s) =>
  //     try
  //       Result.Success(s.toDouble)
  //     catch
  //       case _: NumberFormatException =>
  //         Result.single(FieldError.Mismatch(p, v, "a floating point number"))
  //   case _ => Result.single(FieldError.Mismatch(p, v, "a floating point number"))

  // given Reader[Boolean] = (v, p) => v match
  //   case Str("true") => Result.Success(true)
  //   case Str("false") => Result.Success(false)
  //   case _ => Result.single(FieldError.Mismatch(p, v, "a boolean"))

  given [A](using r: Reader[A]): Reader[Option[A]] = (v, p) => v match
    case Null() => Result.Success(None)
    case _ => r.read(v, p).map(s => Some(s))

  given mapReader[K, V, M[K, V] <: Iterable[(K, V)]](
    using kr: Reader[K],
    vr: Reader[V],
    factory: collection.Factory[(K, V), M[K, V]]
  ): Reader[M[K, V]] = (v, p) => v match
    case Config(fields) =>
      val errors = collection.mutable.ArrayBuffer.empty[FieldError]
      val items = factory.newBuilder
      for (k, v) <- fields do
        kr.read(Str(k), p) match
          case Result.Success(parsedKey) =>
            vr.read(v, p :+ k) match
              case Result.Success(parsedValue) =>
                items += parsedKey -> parsedValue
              case Result.Error(errs) =>
                errors ++= errs
          case Result.Error(errs) => errors ++= errs
      if errors.isEmpty then Result.Success(items.result())
      else Result.Error(errors.toSeq)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected a config map"))


  given colReader[Elem, Col[Elem] <: Iterable[Elem]](
    using elementReader: Reader[Elem],
    factory: collection.Factory[Elem, Col[Elem]]
  ): Reader[Col[Elem]] = (v, p) => v match
    case Arr(elems) =>
      val errors = collection.mutable.ArrayBuffer.empty[FieldError]
      val items = factory.newBuilder
      for (elem, idx) <- elems.zipWithIndex do
        elementReader.read(elem, p :+ idx.toString) match
          case Result.Success(item) => items += item
          case Result.Error(errs) => errors ++= errs
      if errors.isEmpty then Result.Success(items.result())
      else Result.Error(errors.toSeq)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected an array"))


object DerivationMacros:

  import scala.quoted.Expr
  import scala.quoted.Quotes
  import scala.quoted.Type
  def derivedImpl[Api <: ReaderApi : Type, A: Type](using qctx: Quotes)(api: Expr[Api]): Expr[ReaderApi#Reader[A]] =
    import qctx.reflect.*

    val tpe = TypeRepr.of[A]
    if !(tpe <:< TypeRepr.of[Product]) then
      report.error(s"${tpe.show} is not a product type")
      return '{???}

    val constructor = tpe.typeSymbol.companionModule.methodMember("apply").head

    val fieldReaders: List[List[Expr[ReaderApi#Reader[_]]]] =
      for params <- constructor.paramSymss yield
        for param <- params yield
          val readerType = TypeSelect(api.asTerm, "Reader").tpe.appliedTo(param.termRef.widenTermRefByName)
          Implicits.search(readerType) match
            case iss: ImplicitSearchSuccess => iss.tree.asExprOf[ReaderApi#Reader[_]]
            case _ =>
              report.error(s"No ${readerType.show} available for parameter ${param.name}.", param.pos.get)
              '{???}
        end for
      end for

    '{
      (value: Value, path: List[String]) => value match
        case c: Config =>
          val p = $api
          val names: Seq[String] = ${Expr(constructor.paramSymss.flatten.map(_.name))}
          val readers: Seq[p.Reader[_]] = ${Expr.ofSeq(fieldReaders.flatten)}.asInstanceOf[Seq[p.Reader[_]]]

          val errors = collection.mutable.ArrayBuffer.empty[FieldError]
          val ccArgs = new Array[Any](${Expr(constructor.paramSymss.map(_.size).sum)})

          for i <- 0 until ccArgs.size do
            readers(i).read(c.getValue(names(i)), path :+ names(i)) match
              case Result.Success(value) => ccArgs(i) = value
              case Result.Error(errs) => errors ++= errs

          if errors.isEmpty then
            try
              val s = ${
                val base: Term = Ref(constructor)
                var i = 0
                val accesses: List[List[Term]] =
                  for params <- constructor.paramSymss yield
                    for param <- params yield

                      param.termRef.widenTermRefByName.asType match
                        case '[t] =>
                          val expr = '{
                            ccArgs(${Expr(i)}).asInstanceOf[t]
                          }
                          i += 1
                          expr.asTerm
                val application = accesses.foldLeft(base)((lhs, args) => Apply(lhs, args))
                application.asExprOf[A]
              }
              Result.Success(s)
            catch
              case ex: IllegalArgumentException =>
                val err = FieldError.Validation(path, value, ex.getMessage())
                Result.Error(Seq(err))
          else Result.Error(errors.toSeq)

        case _ => Result.single(FieldError.Mismatch(path, value, "a config map"))
    }

trait UnmarshalApi extends ReaderApi:
  def unmarshal[A](cfg: Config, path: List[String] = Nil)(using reader: Reader[A]): A = reader.read(cfg, path) match
    case Result.Success(a) => a
    case Result.Error(errors) =>
      val err = for e <- errors yield e.pretty
      throw ReadException(err.mkString("\n", "\n", ""))

  def unmarshalOrExit[A](
    cfg: Config,
    path: List[String] = Nil,
    stderr: java.io.PrintStream = System.err,
    exit: Int => Nothing = sys.exit
  )(using reader: Reader[A]): A =
    reader.read(cfg, path) match
      case Result.Success(a) => a
      case Result.Error(errs) =>
        for err <- errs do stderr.println(err.pretty)
        exit(1)
