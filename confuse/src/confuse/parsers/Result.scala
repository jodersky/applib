package confuse.parsers

import confuse.model.*

enum Result[+A]:
  case Success(value: A)
  case FieldErrors(errs: Seq[FieldError]) extends Result[Nothing]

object Result:
  def singleError(err: FieldError) = Result.FieldErrors(Seq(err))

enum FieldError:

  /** The given value could not be parsed as the expected type */
  case TypeMismatch(path: Path, value: Value, expected: String)

  /** A requirement failed when instantiating a case class */
  case Validation(path: Path, value: Value, ex: IllegalArgumentException)

  def pretty = this match
    case TypeMismatch(path, value, expected) if value.origins.isEmpty =>
      s"missing required configuration '$path' (expected $expected)"
    case TypeMismatch(path, value, expected) =>
      s"type mismatch in configuration '$path': found ${FieldError.shortTpe(value)}, expected $expected\n${FieldError.origin(value)}"
    case Validation(path, value, ex) =>
      s"validation error in configuration '$path': ${ex.getMessage()}\n${FieldError.origin(value)}"

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
