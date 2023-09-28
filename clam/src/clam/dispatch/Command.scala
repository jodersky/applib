package clam.dispatch

import clam.getopt
import collection.mutable as m
import clam.util.Terminal

class Subcommand[Args](
  val name: String,
  pdefs: Seq[getopt.Param],
  extract: (Context, getopt.Result, Boolean) => Result[Args]
) extends ((Context, Iterator[String]) => Result[Any]):

  private var _action: Option[Args => Result[Any]] = None

  def action(fn: Args => Any): this.type =
    val f = (v: Args) =>
      try
        Result.Success(fn(v))
      catch
        case ex: Exception =>
          Result.InvocatioError(ex)
    _action = Some(f)
    this

  // builtin subcommands
  private val _builtins = m.Map.empty[String, (Context, Iterator[String]) => Result[Any]]

  // call an external (non-builtin command)
  private var _callExternalCommand: Option[(Context, String, Iterator[String]) => Result[Any]] = None
  private var _listExternalCommands: Context => Iterable[String] = _ => Seq()

  private def listCommands(ctx: Context) =
    _builtins.keySet.toSeq ++ _listExternalCommands(ctx)

  private def hasCommand(): Boolean = !_builtins.isEmpty || _callExternalCommand.isDefined

  private def callCommand(ctx: Context, name: String, args: Iterator[String]): Result[Any] =
    if _builtins.contains(name) then
      _builtins(name).apply(ctx, args)
    else if _callExternalCommand.isDefined then
      _callExternalCommand.get.apply(ctx, name, args)
    else
      ctx.reporter.error(s"unknown command (available: ${listCommands(ctx)}")
      Result.ArgumentError()

  def subcommand(name: String, fn: (Context, Iterator[String]) => Result[Any]): this.type =
    _builtins += name -> fn
    this

  def subcommand(scmd: Subcommand[?]): this.type =
    _builtins += scmd.name -> scmd
    this

  def externalCommand(
    call: (Context, String, Iterator[String]) => Result[Any],
    list: Context => Seq[String]
  ): this.type =
    _callExternalCommand = Some(call)
    _listExternalCommands = list
    this


  // def system(name: String)(using w: EnvWriter[Value]): this.type =
  //   externalCommand(
  //     (ctx: Context, name: String, args: Iterator[String]) =>
  //       val env = w.write(ctx.parent)
  //       try
  //         os.proc("")
  //         // os.proc(name, args.toList).call(env = env, check = false)
  //         ???
  //       catch
  //         case _ =>
  //       ???,
  //     ???
  //   )

  //   this

  def apply(ctx: Context, args: Iterator[String]): Result[Any] =
    // if ctx.completionRequested then
    //   sys.error("completion forwarding not yet implemented")

    val res = getopt.parse(pdefs, args)

    // if res.unknown == Some("--help", _) then
    //   ???
    // if res.unknown == Some("--bash-completion", _) then
    //   ???

    val values = extract(ctx, res, hasCommand() && res.unknown.isDefined) match
      case Result.Success(a) => a
      case err => return err

    res.unknown match
      case Some(command, remainder) if hasCommand() =>
        callCommand(ctx, command, remainder)
      case Some(unknown, _)  =>
        ctx.reporter.error("unknown argument")
        Result.ArgumentError()
      case None if _action.isDefined =>
        _action.get(values)
      case None if hasCommand() =>
        ctx.reporter.error("command missing")
        Result.ArgumentError()
      case _ =>
        Result.Success(values) // no action or command set

  def parseOrExit(
    args: Iterable[String],
    reporter: Reporter = Reporter(System.out, System.err),
    terminal: Terminal = Terminal.current(),
    env: Map[String, String] = sys.env,
    exit: Int => Nothing = sys.exit
  ): Any =
    val ctx = Context(reporter, terminal)
    ctx.env ++= env
    apply(ctx, args.iterator) match
      case Result.ArgumentError() => exit(2)
      case _: Result.InvocatioError => exit(1)
      case Result.EarlyExit() => exit(0)
      case Result.Success(a) => a

// object Subcommand:

//   class command() extends annotation.StaticAnnotation
//   class parent() extends annotation.StaticAnnotation

//   @group
//   def bar(git: Git) = ???

//   @parent("git") // env prefixed with => "GIT_"
//   @subcommand("foo")
//   def foo(git: Git)(x: Int) = ???

//   // def findAll[A](container: A)
