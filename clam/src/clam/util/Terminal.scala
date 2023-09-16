package clam.util

/** Information about a terminal. */
case class Terminal(
  rows: Option[Int],
  cols: Option[Int]
)

object Terminal:

  private def run(
    commands: Array[String],
    nlines: Int,
    stdin: String,
  ): Array[String] = {
    val builder = new ProcessBuilder(commands: _*)
    builder.redirectInput(new java.io.File(stdin))
    val process = builder.start()

    val results = new Array[String](nlines)
    val reader = new java.io.BufferedReader(
      new java.io.InputStreamReader(process.getInputStream())
    )
    try {
      for (i <- 0 until nlines) {
        val line = reader.readLine()
        results(i) = line
      }
      results
    } finally {
      process.destroy()
      process.waitFor()
      reader.close()
    }
  }

  /** Properties of the current terminal. */
  def current(): Terminal = Terminal(getRows(), getCols())

  /** Get number of rows in the current terminal.
    *
    * This will return None if the number of rows cannot be determined,
    * for example on an unsupported operating system.
    */
  private def getRows(): Option[Int] = try {
    val result = run(Array("stty", "-a"), 1, "/dev/tty").head
    """rows (\d+)""".r.findFirstMatchIn(result).map(_.group(1).toInt)
  } catch {
    case _: Exception => None
  }

  /** Get number of columns in the current terminal.
    *
    * This will return None if the number of columns cannot be determined,
    * for example on an unsupported operating system.
    */
  private def getCols(): Option[Int] = try {
    val result = run(Array("stty", "-a"), 1, "/dev/tty").head
    """columns (\d+)""".r.findFirstMatchIn(result).map(_.group(1).toInt)
  } catch {
    case _: Exception => None
  }

