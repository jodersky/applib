package test
package ini

import utest._

object NegTest extends DynamicTestSuite {
  import confuse.ini

  testAll(os.pwd / "confuse" / "test" / "resources" / "ini" / "neg", _.ext == "ini"){ inFile =>
    val outFile = inFile / os.up / (inFile.baseName + ".txt")

    val s = os.read.inputStream(inFile)
    try {
      val parser = ini.Parser(s, ini.NoopVisitor)
      val err = intercept[ini.ParseException] {
        parser.parse()
      }
      DiffTools.assertNoDiff(outFile, err.pretty())
    } finally {
      s.close()
    }
  }

}
