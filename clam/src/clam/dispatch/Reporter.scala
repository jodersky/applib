package clam.dispatch

class Reporter(
  val stdout: java.io.PrintStream,
  val stderr: java.io.PrintStream
):
  private var _nerrors: Int = 0

  def error(message: String): Unit =
    _nerrors += 1
    stderr.println(message)

  def warning(message: String): Unit =
    stderr.println(message)

  def errors: Int = _nerrors
  def hasErrors: Boolean = _nerrors > 0
