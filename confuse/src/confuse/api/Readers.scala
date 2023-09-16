package confuse.api

import confuse.Result
import confuse.Value
import confuse.Arr
import confuse.Config
import confuse.Null
import confuse.Str
import confuse.FieldError
import confuse.ReadException

// bug https://github.com/lampepfl/dotty/issues/17201
// import confuse.{Reader => _, *}

trait ReaderApi:
  api =>

  trait Reader[A]:
    def read(value: Value, path: List[String]): Result[A]

  object Reader:
    inline def derived[A]: Reader[A] = derivedImpl[A].asInstanceOf[Reader[A]]
    private inline def derivedImpl[A] = ${
      confuse.macros.derivedImpl[api.type, A]('api)
    }

trait StandardReaders extends ReaderApi with clam.core.StandardStringReaders:
  api =>


  given valueReader: Reader[Value] = (v, p) => Result.Success(v)

  given cfgReader: Reader[Config] = (v, p) => v match
    case c: Config => Result.Success(c)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected a config object"))

  given arrReader: Reader[Arr] = (v, p) => v match
    case a: Arr => Result.Success(a)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected a config array"))

  given strReader: Reader[Str] = (v, p) => v match
    case s: Str => Result.Success(s)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected a string"))

  given nullReader: Reader[Null] = (v, p) => v match
    case n: Null => Result.Success(n)
    case _ => Result.single(FieldError.Mismatch(p, v, "expected an unset field"))

  given direct[A](using sr: StringReader[A]): Reader[A] = (v, p) => v match
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

  given optReader[A](using r: Reader[A]): Reader[Option[A]] = (v, p) => v match
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

    case Null() => Result.Success(factory.newBuilder.result())
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



// trait UnmarshalApi extends ReaderApi:
//   def unmarshal[A](cfg: Config, path: List[String] = Nil)(using reader: Reader[A]): A = reader.read(cfg, path) match
//     case Result.Success(a) => a
//     case Result.Error(errors) =>
//       val err = for e <- errors yield e.pretty
//       throw ReadException(err.mkString("\n", "\n", ""))

//   def unmarshalOrExit[A](
//     cfg: Config,
//     path: List[String] = Nil,
//     stderr: java.io.PrintStream = System.err,
//     exit: Int => Nothing = sys.exit
//   )(using reader: Reader[A]): A =
//     reader.read(cfg, path) match
//       case Result.Success(a) => a
//       case Result.Error(errs) =>
//         for err <- errs do stderr.println(err.pretty)
//         exit(1)
