package clam


enum Result[+A]:
  case Success(a: A)

  /** An error occurred while parsing arguments. The exact errors should be
    * reported via a reporter. */
  case ArgumentError extends Result[Nothing]

  /** An error occurred while parsing arguments */
  case EarlyExit extends Result[Nothing]
