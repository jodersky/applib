import utest._

import clam.dispatch.Subcommand
import clam.dispatch.Result
import clam.getopt

object DispatchTest extends TestSuite:

  val entrypoint = Subcommand[Option[String]](
    "git-foo",
    Seq(),
    (ctx, res, hasNext) =>
      val config = ctx.env.get("CONFIG")
      Result.Success(config)
  )

  val launcher = Subcommand[Option[String]](
    "git",
    Seq(getopt.Param("--config", Seq(), false, false, false)),
    (ctx, res, hasNext) =>
      val config: Option[String] = res.knownArgs("--config").headOption.map(_.arg.get)
      if hasNext && config.isDefined then
        ctx.env += "CONFIG" -> config.get
      Result.Success(config)
  ).subcommand("foo", entrypoint)

  val tests = Tests{
    test("call via command chain") {
      val res = launcher.parseOrExit(Seq("--config=1", "foo"), exit = n => sys.error("error"))
      res ==> Some("1")
    }
    test("call directly") {
      val res = entrypoint.parseOrExit(Seq(), env = Map("CONFIG" -> "1"), exit = n => sys.error("error"))
      res ==> Some("1")
    }

  }

