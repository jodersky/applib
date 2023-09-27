package clam.readers


/** Defines the reader type class as a member of an API trait.
  *
  * Making readers path-dependent is done so that implicit lookup for derivation
  * can be customized in one central place, without resorting to imports in
  * various places.
  */
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

