package clam.dispatch

enum Result[+A]:
  case Success(a: A)
  case ArgumentError() extends Result[Nothing]
  case EarlyExit() extends Result[Nothing]

  case InvocationError(t: Throwable) extends Result[Nothing]

  def isOk: Boolean = this match
    case Success(_) => true
    case EarlyExit() => true
    case _ => false

