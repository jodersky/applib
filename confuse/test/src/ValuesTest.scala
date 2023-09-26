package test

import utest._

object ValuesTest extends DynamicTestSuite {

  testAll(os.pwd / "confuse" / "test" / "resources" / "values"){ inFile =>
    val outFile = inFile / os.up / (inFile.baseName + ".out")

    val cfg = confuse.read(Seq(inFile.relativeTo(os.pwd)))

    val b = StringBuilder()
    for (key, value) <- cfg.flatten().toSeq.sortBy(_._1) do
      b ++= key
      b ++= "="
      value match
        case confuse.Str(s) =>
          b ++= "'"
          b ++= s
          b ++= "'"
        case confuse.Null() =>
          b ++= "null"
      b ++= "\n"

    DiffTools.assertNoDiff(outFile, b.result())
  }

}
