package clam


enum Result[+A]:
  case Success(a: A)

  /** An error occurred while parsing arguments. The exact errors should be
    * reported via a reporter. */
  case ArgumentError extends Result[Nothing]

  /** An error occurred while parsing arguments */
  case EarlyExit extends Result[Nothing]


// sealed trait Result[+A]

// object Result:

//   /** Parsing succeeded */
//   case class Success[A](value: A) extends Result[A]

//   /** There were errors parsing the arguments. */
//   case class ArgumentErrors(
//     errors: Iterable[ArgumentError]
//   ) extends Result[Nothing]

//   /** There were no errors parsing the arguments, but some side-effecting parameter
//     * prevented the parsing from continuing.
//     */
//   case class EarlyExit() extends Result[Nothing]

// enum ArgumentError:
//   /** The argument was encountered but not specified as a parameter a priori. */
//   case Unknown(arg: String)

//   /** The argument is required and was not encountered on the command line. */
//   case Missing(arg: String)

//   /** The (named) argument itself takes an argument which was not specified. */
//   case Incomplete(arg: String)

//   /** The argument could not be parsed to the expected type. */
//   case ParseError(arg: String, message: String)

//   /** The subcommand was encountered but does not match a valid option. */
//   case UnknownCommand(arg: String)

//   /** The subcommand is required but but was not encountered. */
//   case MissingCommand()

//   def pretty() = this match
//     case Unknown(arg) => s"unknown argument: $arg"
//     case Missing(arg) => s"missing argument: $arg"
//     case Incomplete(arg) => s"argument $arg is incomplete: expected an argument"
//     case ParseError(arg, message) => s"error parsing $arg: $message"
//     case UnknownCommand(arg) => s"'$arg' is not a valid command"
//     case MissingCommand() => s"expected command"

class param(
  val name: String = null,
  val aliases: Seq[String] = Seq(),
  val flag: Boolean = false,
  val endOfNamed: Boolean = false,
  val interactiveCompleter: String => Seq[String] = null,
  val standaloneCompleter: BashCompleter = null
) extends annotation.StaticAnnotation
