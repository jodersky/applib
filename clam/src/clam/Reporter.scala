package clam

class Reporter(
  val stdout: java.io.PrintStream,
  val stderr: java.io.PrintStream,
  prefix: String
):
  private var _nerrors: Int = 0

  def error(message: String): Unit =
    _nerrors += 1
    stderr.println(prefix + ": " + message)

  def warning(message: String): Unit =
    stderr.println(prefix + ": " + message)

  def errors: Int = _nerrors
  def hasErrors: Boolean = _nerrors > 0
