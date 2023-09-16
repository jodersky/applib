package clam.readers


/** Defines the reader type class as a member of an API trait. This is done so
  * that typeclass instances can be customized within macro expansions. */
trait ReaderApi:

  trait Reader[A]:
    def read(arg: String): Reader.Result[A]
    def typeName: String

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

