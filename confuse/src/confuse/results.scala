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
