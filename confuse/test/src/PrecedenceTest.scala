package test

import utest._

object PrecedenceTest extends TestSuite {

  val tests = Tests {
    test("precedence") {
      val root = os.pwd / "confuse" / "test" / "resources" / "precedence"
      val cfg = confuse.read(
        Seq(os.sub / "dir", os.sub / "file.json"),
        pwd = root,
        env = Map(
          "UNUSED" -> "ok",
          "APP_ENV_A" -> "env",
          "APP_ENV_B" -> "env",
          "EXTRA_COOL" -> "env"
        ),
        envPrefix = "APP_",
        envBinds = Seq("EXTRA_COOL" -> "env.c", "UNSET" -> "env.e"),
        props = Map(
          "unused" -> "ok",
          "app.props.a" -> "props",
          "app.props.b" -> "props",
          "extra.cool" -> "props"
        ),
        propsPrefix = "app.",
        propsBinds = Seq("extra.cool" -> "props.c", "unset" -> "props.e")
      )

      val outFile = os.pwd / "confuse" / "test" / "resources" / "precedence" / "dump.out"
      DiffTools.assertNoDiff(outFile, cfg.dump())
    }
  }

}
