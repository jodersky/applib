package clam

trait ReaderApi:

  trait Reader[A]:
    def read(arg: String): Reader.Result[A]

  object Reader:
    enum Result[+A]:
      case Success(value: A)
      case Error(message: String) extends Result[Nothing]

      def map[B](fn: A => B): Result[B] = this match
        case Success(value) => Success(fn(value))
        case err: Error => err
      def flatMap[B](fn: A => Result[B]): Result[B] = this match
        case Success(value) => fn(value)
        case err: Error => err

trait StandardReaders extends ReaderApi with clam.core.StandardStringReaders:
  import Reader.Result

  given [A](using r: StringReader[A]): Reader[A] with
    def read(arg: String): Result[A] = r.read(arg) match
      case StringReader.Result.Success(a) => Result.Success(a)
      case StringReader.Result.Error(a) => Result.Error(a)


  // given stringReader: Reader[String] = s => Reader.Result.Success(s)

  // given booleanReader: Reader[Boolean] with
  //   def read(arg: String) = arg match
  //     case "true" => Result.Success(true)
  //     case "false" => Result.Success(false)
  //     case _ => Result.Error("not a boolean")

  // given integralReader[N](using n: Integral[N]): Reader[N] with
  //   def read(arg: String) = n.parseString(arg) match
  //     case Some(value) => Result.Success(value)
  //     case None => Result.Error("not an integral number")
