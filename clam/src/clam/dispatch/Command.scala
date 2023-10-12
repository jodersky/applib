package clam.dispatch

import clam.getopt
import clam.completion
import collection.mutable as m
import clam.util.Terminal
import clam.util.Levenshtein


/** An entrypoint to the application
  *
  * When a command is called, it goes through two phases:
  *
  * 1. argument parsing (this may alter the available subcommands)
  * 2. running an action or subcommand
  *
  * A command can be in a tree, but it is designed to be callable as a top-level
  * entrypoint. As such, parent args must be stored in a global context, usually
  * the environment.
  *
  * When a command is called, it will first parse arguments depending on its
  * parameter definitions. Then, depending on the remainder, a subcommand may be
  * called.
  *
  * @param pdefs The parameter definitions
  * @param extract A function that is called with the command's arguments, and
  * whose result may be passed to an action if one is defined. The arguments
  * are: (context, result, next), where:
  *   - context: the global (mutable) context. Values may be passed to
  *     subcommands by putting them in this context
  *   - result: the getopt parameter result from the pdefs
  *   - next: whether or not a subcommand will be called
  * @param action A function to call with the extracted result
  * @param builtins Subcommand candidates to call
  */
class Command(
  val name: String,
  val description: String = "",
  val pdefs: m.ArrayBuffer[ParamDef] = m.ArrayBuffer.empty,
  var parse: (Command, Context, getopt.Result) => Result[Any] = (_,_, _) => Result.EarlyExit(),
  var action: Option[Any => Result[Any]] = None,
  val builtins: m.Map[String, Command] = m.Map(),
  var listExternalCommands: () => Iterable[String] = () => Seq(),
  var callExternalCommand: Option[(Context, Iterator[String]) => String => Result[Any]] = None,
  var makeHelp: (Command, Context) => String = Command.defaultHelpMessage
) extends ((Context, Iterator[String]) => Result[Any]):
  private def hasSubcommand = builtins.nonEmpty || callExternalCommand.isDefined

  def apply(ctx: Context, args: Iterator[String]): Result[Any] =
    if completeOrFalse(ctx) then return Result.EarlyExit()

    val opts = getopt.parse(pdefs.map(_.parseInfo), args)

    val parsed = parse(
      this,
      ctx,
      opts
    ) match
      case Result.Success(a) => a
      case err => return err

    opts.unknown match
      case Some(command, remainder) if hasSubcommand =>
        if builtins.contains(command) then
          builtins(command).apply(ctx, remainder)
        else if callExternalCommand.isDefined then
          callExternalCommand.get.apply(ctx, remainder)(command)
        else
          ctx.stderr.println(s"unknown command '$command'")
          val similar = Levenshtein.closest(command, builtins.keySet ++ listExternalCommands())
          if !similar.isEmpty then
            ctx.stderr.println("similar commands:")
            for s <- similar do
              ctx.stderr.println("  " + s)
          Result.ArgumentError()
      case Some(unknown, _)  =>
        ctx.stderr.println(s"unknown argument $unknown")
        Result.ArgumentError()
      case None if action.isDefined =>
        action.get(parsed)
      case None if hasSubcommand =>
        ctx.stderr.println("command missing")
        Result.ArgumentError()
      case _ =>
        Result.Success(parsed) // no action or command set

  def parse(
    args: Iterable[String],
    stdout: java.io.PrintStream = System.out,
    stderr: java.io.PrintStream = System.err,
    env: collection.Map[String, String] = sys.env,
    terminal: Terminal = Terminal.current(),
    exit: Int => Nothing = sys.exit
  ): Result[Any] =
    val ctx = Context(stdout, stderr, m.Map.from(env), terminal)
    apply(ctx, args.iterator)

  def parseOrExit(
    args: Iterable[String],
    stdout: java.io.PrintStream = System.out,
    stderr: java.io.PrintStream = System.err,
    env: collection.Map[String, String] = sys.env,
    terminal: Terminal = Terminal.current(),
    exit: Int => Nothing = sys.exit
  ): Any =
    parse(args, stdout, stderr, env, terminal) match
      case Result.ArgumentError() => exit(2)
      case Result.InvocationError(_) => exit(1)
      case Result.EarlyExit() => exit(0)
      case Result.Success(a) => a

  def printHelp(ctx: Context): Unit =
    ctx.stdout.println(makeHelp(this, ctx))

  def printCommandCompletion(ctx: Context): Unit =
    ctx.env.put("GEN_COMPLETION", name)
    completeOrFalse(ctx)

  private def completeOrFalse(ctx: Context): Boolean =
    val commandChain = ctx.env.get("GEN_COMPLETION").map(_.split(",")) match
      case Some(parts) => parts.toSeq
      case None => return false

    val extra = if builtins.isEmpty then Seq() else
      Seq(completion.Param(Seq("command"), false, false, completion.BashCompleter.Fixed(builtins.keySet)))

    for (name, child) <- builtins do
      ctx.env.put("GEN_COMPLETION", (commandChain :+ name).mkString(","))
      child.completeOrFalse(ctx)

    completion.StandaloneBashCompletion.printCommandCompletion(
      commandChain,
      pdefs.map(_.completionInfo).toSeq ++ extra,
      ctx.stdout
    )
    true


object Command:
  import clam.text

  /** Generate a help message from parameters.
    *
    * Overriding this allows you to customize the help message of all commands.
    */
  def defaultHelpMessage(
    cmd: Command,
    ctx: Context
  ): String = {
    val term = ctx.terminal
    val (named0, positional) = cmd.pdefs.toSeq.partition(_.isNamed)
    val named = named0.sortBy(_.names.head)

    val b = new StringBuilder
    b ++= s"Usage: "
    b ++= cmd.name

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
        if (param.repeats) b ++= "..."
        b ++= "]"
      }
    }

    for (param <- positional) {
      b ++= " <"
      b ++= param.names.head
      b ++= ">"
      if (param.repeats) b ++= "..."
    }

    if (!cmd.builtins.isEmpty) {
      b ++= " <command> [<args>]"
    }
    b ++= "\n"

    if (!cmd.description.isEmpty()) {
      b ++= "\n"
      b ++= cmd.description
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

    if (!cmd.builtins.isEmpty) {
      val width = cmd.builtins.map(_._1.length).max + 3

      b ++= "Commands:\n"
      for ((name, scmd) <- cmd.builtins) {
        b ++= "  "
        b ++= name
        b ++= " "
        b ++= " " * (width - name.length)
        text.wrap(scmd.description, b, term.cols.getOrElse(80) - width, "\n" + " " * width)
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
