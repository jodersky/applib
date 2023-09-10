case class Output(stream: java.io.PrintStream, color: Boolean):
  def println(message: Any) =
    if color then
      stream.print(Console.RED)
      stream.println(message)
      stream.print("\u001b[39m")
    else
      stream.println(message)

object Output:
  given clam.Parser[Output] with
    def paramDefs: Vector[clam.GetOpt.Param] = Vector(
      clam.GetOpt.Param("--no-color", flag = true)
    )
    def paramInfos: Vector[clam.ParamInfo] = Vector(
      clam.ParamInfo(true, Seq("--no-color"), argName = None, repeats = false, "no color", _ => Seq(), clam.BashCompleter.Empty)
    )
    def subcommands: Map[String, clam.Command[?]] = Map.empty
    def extract(ctx: clam.ParseContext, reporter: clam.Reporter, args: clam.GetOpt.Result) =
      val shouldColor =
        args.knownArgs("--no-color").isEmpty &&
        sys.env.get("TERM").isDefined &&
        sys.env("TERM") != "dumb"

      clam.Result.Success(Output(System.out, shouldColor))

case class Verbosity(level: Int)
object Verbosity:

  given clam.Parser[Verbosity] with
    def paramDefs: Vector[clam.GetOpt.Param] = Vector(
      clam.GetOpt.Param("-v", flag = true)
    )
    def paramInfos: Vector[clam.ParamInfo] = Vector(
      clam.ParamInfo(true, Seq("-v"), argName = None, repeats = false, "verbosity", _ => Seq(), clam.BashCompleter.Empty)
    )
    def subcommands: Map[String, clam.Command[?]] = Map.empty
    def extract(ctx: clam.ParseContext, reporter: clam.Reporter, args: clam.GetOpt.Result) =
      clam.Result.Success(Verbosity(args.knownArgs("-v").size))

/** An example application
  *
  * @param server The server to connect to
  * @param config Configuration files to use
  * @param define Explicit config definitions
  */
case class Cli(
  output: Output,
  verbosity: Verbosity,
  server: String = "localhost",
  @clam.param(aliases = Seq("-c")) config: Seq[os.FilePath] = Seq(),
  @clam.param(aliases = Seq("-D")) define: Seq[(String, String)] = Seq()
) derives clam.Command


case class Settings(
  x: Int,
  // date: java.time.Instant
) derives confuse.Reader


def main(args: Array[String]): Unit =
  val cli = clam.Command[Cli].parseOrExit(args)
  val config = confuse.default.read(paths = cli.config, args = cli.define)
  cli.output.println(cli.verbosity.level)

  cli.output.println(config.dump())

  val settings = confuse.default.unmarshalOrExit[Settings](config)

