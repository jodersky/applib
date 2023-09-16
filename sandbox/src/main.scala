// case class Output(stream: java.io.PrintStream, color: Boolean):
//   def println(message: Any) =
//     if color then
//       stream.print(Console.RED)
//       stream.println(message)
//       stream.print("\u001b[39m")
//     else
//       stream.println(message)

// object Output:
//   given clam.Parser[Output] with
//     def paramDefs: Vector[clam.GetOpt.Param] = Vector(
//       clam.GetOpt.Param("--no-color", flag = true)
//     )
//     def paramInfos: Vector[clam.ParamInfo] = Vector(
//       clam.ParamInfo(true, Seq("--no-color"), argName = None, repeats = false, "no color", _ => Seq(), clam.BashCompleter.Empty)
//     )
//     def subcommands: Map[String, clam.Command[?]] = Map.empty
//     def extract(ctx: clam.ParseContext, reporter: clam.Reporter, args: clam.GetOpt.Result) =
//       val shouldColor =
//         args.knownArgs("--no-color").isEmpty &&
//         sys.env.get("TERM").isDefined &&
//         sys.env("TERM") != "dumb"

//       clam.Result.Success(Output(System.out, shouldColor))

// case class Verbosity(level: Int)
// object Verbosity:

//   given clam.Parser[Verbosity] with
//     def paramDefs: Vector[clam.GetOpt.Param] = Vector(
//       clam.GetOpt.Param("-v", flag = true)
//     )
//     def paramInfos: Vector[clam.ParamInfo] = Vector(
//       clam.ParamInfo(true, Seq("-v"), argName = None, repeats = false, "verbosity", _ => Seq(), clam.BashCompleter.Empty)
//     )
//     def subcommands: Map[String, clam.Command[?]] = Map.empty
//     def extract(ctx: clam.ParseContext, reporter: clam.Reporter, args: clam.GetOpt.Result) =
//       clam.Result.Success(Verbosity(args.knownArgs("-v").size))

/** An example application
  *
  * @param server The server to connect to
  * @param config Configuration files to use
  * @param define Explicit config definitions
  */
case class Cli(
  server: String = "localhost",

  @clam.param(aliases = Seq("-c"))
  configs: Seq[os.FilePath] = Seq(),

  @clam.param(aliases = Seq("-D"))
  defines: Seq[(String, String)] = Seq(),

  cmd: Subcommand
) derives clam.Command

sealed trait Subcommand derives clam.Subcommand:
  def run(cli: Cli): Unit

/** Print version information */
case class Version() extends Subcommand:
  def run(cli: Cli) = println("5")

case class Run() extends Subcommand:
  def run(cli: Cli) = println("runme")

case class Info() extends Subcommand:
  def run(cli: Cli) = println(cli)


// enum Subcommand derives clam.Subcommand:
//   // def run(): Unit = ???


//   /** Run me */
//   case Run(x: Int)

//   /** Run me */
//   case Ra(x: Int)

//   /** Run me */
//   case Add(x: Int)

//     /** Show version */
//   case Version()

// case class Settings(
//   x: Int = 2,
//   data: Map[String, String]
// ) derives confuse.default.Reader

def main(args: Array[String]): Unit =
  val cli = clam.parseOrExit[Cli](args)
  cli.cmd.run(cli)
  // println(cli)
  // val config = confuse.default.read(paths = cli.config, args = cli.define)
  // println(config.dump())
  // val settings = config.unmarshalOrExit[Settings]()

  // println(settings)
