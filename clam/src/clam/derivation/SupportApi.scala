package clam.derivation


import clam.util.Terminal
import clam.text

trait SupportApi:

  /** Generate a help message from parameters.
    *
    * Overriding this allows you to customize the help message of all commands.
    */
  def defaultHelpMessage(
    term: Terminal,
    description: String,
    commandChain: Iterable[String],
    params: Iterable[ParamDef],
    subcommands: Iterable[(String, String)]
  ): String = {
    val (named0, positional) = params.toSeq.partition(_.isNamed)
    val named = named0.sortBy(_.names.head)

    val b = new StringBuilder
    b ++= s"Usage:"
    for (name <- commandChain) {
      b ++= " "
      b ++= name
    }

    // if (!named.isEmpty) {
    //   b ++= " [OPTIONS]"
    // }

    if (!named.isEmpty) {
      for (param <- named) {
        b ++= " ["
        b ++= param.names.head

        for (n <- param.names.tail) {
          b ++= "|"
          b ++= n

        }
        b ++= "]"
      }
    }

    for (param <- positional) {
      b ++= " <"
      b ++= param.names.head
      b ++= ">"
      if (param.repeats) b ++= "..."
    }

    if (!subcommands.isEmpty) {
      b ++= " <command> [<args>]"
    }
    b ++= "\n"

    if (!description.isEmpty()) {
      b ++= "\n"
      b ++= description
      b ++= "\n\n"
    }

    // Note that not necessarily all named parameters must be optional. However
    // since that is usually the case, this is what the default help message
    // assumes.
    if (!named.isEmpty) {
      b ++= "Options:\n"

      // -short, --long tpe wrapped
      val lhs = for (param <- named) yield {
        val long = param.names.head
        val short = if (long.length == 2) "" else param.names.find(_.length == 2).getOrElse("")
        val argname = param.argName.getOrElse("")

        if (short != "") {
          s"  $short, $long $argname  "
        } else {
          s"      $long $argname  "
        }
      }

      val col1Width = lhs.map(_.length).max
      val col2Width = term.cols.getOrElse(80) - col1Width

      if (col2Width > 30) {
        for ((l, param) <- lhs.zip(named)) {
          b ++= l
          b ++= " " * (col1Width - l.length)
          text.wrap(param.description, b, col2Width, "\n" + " " * col1Width)
          b += '\n'
        }
      } else {
        for ((l, param) <- lhs.zip(named)) {
          b ++= l
          b += '\n'
          b ++= param.description
          b += '\n'
        }
      }
    }

    if (!subcommands.isEmpty) {
      val width = subcommands.map(_._1.length).max + 3

      b ++= "Commands:\n"
      for ((name, cmdDesc) <- subcommands) {
        b ++= "  "
        b ++= name
        b ++= " "
        b ++= " " * (width - name.length)
        text.wrap(cmdDesc, b, term.cols.getOrElse(80) - width, "\n" + " " * width)
        b ++= "\n"
      }
    }

    // val describedPos = positional.filter(!_.description.isEmpty)
    // if (!describedPos.isEmpty) {
    //   b ++= "positional arguments:\n"
    //   for (param <- positional) {
    //     b ++= s"  ${param.names.head}\n        "
    //     TextUtils.wrap(param.description, b, width, "\n        ")
    //     b ++= "\n"
    //   }
    // }

    b.result()
  }
