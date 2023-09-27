package clam.dispatch

enum Result[+A]:
  case Success(a: A)
  case ArgumentError() extends Result[Nothing]
  case EarlyExit() extends Result[Nothing]

  case InvocatioError(t: Throwable) extends Result[Nothing]
