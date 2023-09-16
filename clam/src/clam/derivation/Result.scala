package clam.derivation

enum Result[+A]:
  case Success(a: A)

  /** An error occurred while parsing arguments. The exact errors should be
    * reported via a reporter. */
  case ArgumentError extends Result[Nothing]

  /** AAn argument was encountered that requested immediate termination, but
    * which is not an error (e.g.`--help`). */
  case EarlyExit extends Result[Nothing]
