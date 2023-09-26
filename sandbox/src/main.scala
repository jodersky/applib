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

sealed trait Subcommand derives clam.Subcommand

/** Print version information */
case class Version() extends Subcommand

case class Run() extends Subcommand

case class Info() extends Subcommand

case class Foo(
  name: String,
  value: Int = 42
) derives confuse.default.Parser
case class Settings(
  x: Int = 2,
  data: Map[String, Foo]
) derives confuse.default.Parser

def main(args: Array[String]): Unit =
  val cli = clam.parseOrExit[Cli](args)
  println(cli)

  val config = confuse.read(paths = cli.configs, args = cli.defines)
  println("raw config result")
  println(config.dump())


  val settings = config.parseOrExit[Settings]()
  println(settings)
