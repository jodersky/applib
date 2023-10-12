package clam.core

trait StringReaderApi:

  trait StringReader[A]:
    def read(str: String): StringReader.Result[A]
    def typeName: String

  object StringReader:
    enum Result[+A]:
      case Success(a: A)
      case Error(message: String)

trait StandardStringReaders extends StringReaderApi with LowPrioStringReaders:
  import StringReader.Result

  given stringReader: StringReader[String] with
    def read(s: String) = Result.Success(s)
    def typeName: String = "string"

  given booleanReader: StringReader[Boolean] with
    def read(s: String) = s match
      case "true" => Result.Success(true)
      case "false" => Result.Success(false)
      case _ => Result.Error("not a boolean")
    def typeName: String = "boolean"

  given integralReader[N](using n: Integral[N]): StringReader[N] with
    def read(s: String) = n.parseString(s) match
      case Some(value) => Result.Success(value)
      case None => Result.Error("not an integral number")
    def typeName: String = "integer"

  given floatReader: StringReader[Float] with
    def read(s: String) =
      try
        Result.Success(s.toFloat)
      catch
        case _: NumberFormatException => Result.Error(s"not a number")
    def typeName: String = "float"

  given doubleReader: StringReader[Double] with
    def read(s: String) =
      try
        Result.Success(s.toDouble)
      catch
        case _: NumberFormatException => Result.Error(s"not a number")
    def typeName: String = "float"

  given pathReader: StringReader[os.FilePath] with
    def read(a: String) =
      try
        Result.Success(os.FilePath(a))
      catch
        case _: IllegalArgumentException =>
          Result.Error("not a valid path")
    def typeName = "path"

  given absPathReader: StringReader[os.Path] with
    def read(a: String) =
      try
        Result.Success(os.Path(a))
      catch
        case _: IllegalArgumentException =>
          Result.Error("not a valid absolute path")
    def typeName = "absolute path"

  given relPathReader: StringReader[os.RelPath] with
    def read(a: String) =
      try
        Result.Success(os.RelPath(a))
      catch
        case _: IllegalArgumentException =>
          Result.Error("not a valid relative path")
    def typeName = "relative path"

  given subPathReader: StringReader[os.SubPath] with
    def read(a: String) =
      try
        Result.Success(os.SubPath(a))
      catch
        case _: IllegalArgumentException =>
          Result.Error("not a valid sub path")
    def typeName = "sub path"

  given jPathReader: StringReader[java.nio.file.Path] with
    def read(a: String) =
      try
        Result.Success(java.nio.file.Paths.get(a))
      catch
        case _: java.nio.file.InvalidPathException => Result.Error("not a path")
    def typeName: String = "path"

  given jFileReader: StringReader[java.io.File] with
    def read(a: String) =
      try
        Result.Success(java.io.File(a))
      catch
        case _: Exception => Result.Error("not a path")
    def typeName: String = "path"

  // private class ColonSeparatedReader[E, Col[_] <: Iterable[_]](tn: String)(using er: StringReader[E], factory: collection.Factory[E, Col[E]]) extends StringReader[Col[E]]:
  //   def read(str: String) =
  //     val parts = str.split(":")
  //     val builder = factory.newBuilder
  //     var i = 0
  //     while i < parts.length do
  //       er.read(parts(i)) match
  //         case Result.Success(elem) => builder += elem
  //         case Result.Error(e) => return Result.Error(e)
  //       i += 1
  //     Result.Success(builder.result())
  //   def typeName: String = tn

  // given pathCollectionReader[Col[_] <: Iterable[os.FilePath]]
  //   (using collection.Factory[os.FilePath, Col[os.FilePath]], StringReader[os.FilePath]): StringReader[Col[os.FilePath]] =
  //     ColonSeparatedReader("path list")

  // given absPathCollectionReader[Col[_] <: Iterable[os.Path]]
  //   (using collection.Factory[os.Path, Col[os.Path]], StringReader[os.Path]): StringReader[Col[os.Path]] =
  //     ColonSeparatedReader("absolute path list")

  // given relPathCollectionReader[Col[_] <: Iterable[os.RelPath]]
  //   (using collection.Factory[os.RelPath, Col[os.RelPath]], StringReader[os.RelPath]): StringReader[Col[os.RelPath]] =
  //     ColonSeparatedReader("relative path list")

  // given subPathCollectionReader[Col[_] <: Iterable[os.SubPath]]
  //   (using collection.Factory[os.SubPath, Col[os.SubPath]], StringReader[os.SubPath]): StringReader[Col[os.SubPath]] =
  //     ColonSeparatedReader("sub path list")

  // given jPathCollectionReader[Col[_] <: Iterable[java.nio.file.Path]]
  //   (using collection.Factory[java.nio.file.Path, Col[java.nio.file.Path]], StringReader[java.nio.file.Path]): StringReader[Col[java.nio.file.Path]] =
  //     ColonSeparatedReader("path list")

  // given jFileCollectionReader[Col[_] <: Iterable[java.io.File]]
  //   (using collection.Factory[java.io.File, Col[java.io.File]], StringReader[java.io.File]): StringReader[Col[java.io.File]] =
  //     ColonSeparatedReader("path list")

  given tupleReader[K, V](using kr: StringReader[K], vr: StringReader[V]): StringReader[(K, V)] with
    def read(a: String): Result[(K, V)] =
      a.split("=", 2) match
        case Array(k, v) =>
          val k1 = kr.read(k)
          val v1 = vr.read(v)
          (k1, v1) match
            case (Result.Success(k2), Result.Success(v2)) => Result.Success((k2, v2))
            case (Result.Error(msg), _)            => Result.Error(msg)
            case (Result.Success(_), Result.Error(msg))   => Result.Error(msg)
        case Array(k) => Result.Error(s"expected value after key '$k'")
        case _        => Result.Error(s"expected key=value pair")
    def typeName = s"${kr.typeName}=${vr.typeName}"

  given optionReader[A](using elementReader: StringReader[A]): StringReader[Option[A]] with
    def read(a: String) =
      elementReader.read(a) match
        case Result.Error(message) => Result.Error(message)
        case Result.Success(value) => Result.Success(Some(value))
    def typeName = elementReader.typeName

  given durationReader: StringReader[scala.concurrent.duration.Duration] with
    def read(a: String) = try
      Result.Success(scala.concurrent.duration.Duration.create(a))
    catch
      case _: NumberFormatException => Result.Error("not a valid duration")
    def typeName = "duration"

  given finiteDurationReader: StringReader[scala.concurrent.duration.FiniteDuration] with
    def read(a: String) = durationReader.read(a) match
      case Result.Success(f: scala.concurrent.duration.FiniteDuration) => Result.Success(f)
      case Result.Success(f: scala.concurrent.duration.Duration) =>
        Result.Error(s"expected a finite duration, but '$a' is infinite")
      case Result.Error(msg) => Result.Error(msg)
    def typeName = "finite duration"

  given instantReader: StringReader[java.time.Instant] with
    def read(a: String) = try
      Result.Success(java.time.Instant.parse(a))
    catch
      case ex: java.time.format.DateTimeParseException =>
        Result.Error("not a valid instant in time. The format must follow 'YYYY-MM-DDThh:mm:ss[.S]Z'. Note that the 'T' is literal and the time zone Z must be given.")

    def typeName = "timestamp"

  given zonedDateTimeReader: StringReader[java.time.ZonedDateTime] with
    def read(a: String) = try
      Result.Success(java.time.ZonedDateTime.parse(a))
    catch
      case ex: java.time.format.DateTimeParseException =>
        Result.Error("not a zoned date and time")
    def typeName = "timestamp"
  given LocalDateTimeReader: StringReader[java.time.LocalDateTime] with
    def read(a: String) = try
      Result.Success(java.time.LocalDateTime.parse(a))
    catch
      case ex: java.time.format.DateTimeParseException =>
        Result.Error("not local date and time")
    def typeName = "local timestamp"

  given LocalDateReader: StringReader[java.time.LocalDate] with
    def read(a: String) = try
      Result.Success(java.time.LocalDate.parse(a))
    catch
      case ex: java.time.format.DateTimeParseException =>
        Result.Error("not parse a local date")

    def typeName = "local date"

  given LocalTime: StringReader[java.time.LocalTime] with
    def read(a: String) = try
      Result.Success(java.time.LocalTime.parse(a))
    catch
      case ex: java.time.format.DateTimeParseException =>
        Result.Error("not a local time")

    def typeName = "local time"

  given RangeReader: StringReader[Range] with
    def read(str: String) = str.split("\\.\\.") match
      case Array(from, to) =>
        try
          Result.Success(from.toInt to to.toInt)
        catch
          case _: Exception => Result.Error("not a numeric range")

      case _ => Result.Error("expected 'from..to'")

    def typeName = "from..to"

trait LowPrioStringReaders extends StringReaderApi:
  import StringReader.Result

  given collectionReader[Elem, Col[Elem] <: Iterable[Elem]](
    using elementReader: StringReader[Elem],
    factory: collection.Factory[Elem, Col[Elem]]
  ): StringReader[Col[Elem]] with
    def read(a: String) =
      val parts = a.split(",")
      val builder = factory.newBuilder

      var i = 0
      while i < parts.length do
        elementReader.read(parts(i)) match
          case Result.Success(a) => builder += a
          case Result.Error(message) => return Result.Error(message)
        i += 1
      Result.Success(builder.result())

    def typeName = s"list of ${elementReader.typeName}s separated by ','"
