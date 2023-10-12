import utest._

import clam.dispatch.Command
import clam.dispatch.Result
import clam.dispatch.ParamDef
import clam.getopt
import collection.mutable as m

object DispatchTest extends TestSuite:

  val entrypoint = Command("foo-config")
  entrypoint.parse = (_, ctx, res) =>
    val config = ctx.env.get("CONFIG")
    Result.Success(config)

  val launcher = Command("foo")
  launcher.pdefs += ParamDef(
    Seq("--config"),
    "config path",
    Some("file")
  )
  launcher.pdefs += ParamDef(
    Seq("--bash-completion"),
    "generate completion",
    None,
  )
  launcher.pdefs += ParamDef(
    Seq("--help"),
    "show help",
    None
  )
  launcher.parse = (cmd, ctx, res) =>
    if res.knownArgs("--help").nonEmpty then
      cmd.printHelp(ctx)
      Result.EarlyExit()
    else if res.knownArgs("--bash-completion").nonEmpty then
      cmd.printCommandCompletion(ctx)
      Result.EarlyExit()
    else
      val config: Option[String] = res.knownArgs("--config").headOption.map(_.arg.get)
      if config.isDefined then
        ctx.env += "CONFIG" -> config.get
      Result.Success(config)
  launcher.builtins += "config" -> entrypoint

  val tests = Tests{
    test("call via command chain") {
      val res = launcher.parseOrExit(Seq("--config=1", "config"), exit = n => sys.error("error"))
      res ==> Some("1")
    }
    test("call directly") {
      val res = entrypoint.parseOrExit(Seq(), env = Map("CONFIG" -> "1"), exit = n => sys.error("error"))
      res ==> Some("1")
    }
    test("help") {
      launcher.parse(Seq("--help")).isOk ==> true
    }
    test("completion") {
      launcher.parse(Seq("--bash-completion")).isOk ==> true
    }

  }

