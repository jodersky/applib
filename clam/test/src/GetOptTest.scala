import utest._

import clam.GetOpt
import clam.GetOpt.Param
import clam.GetOpt.Arg
import collection.mutable.LinkedHashMap
import collection.mutable.ArrayBuffer

object GetOptTest extends TestSuite:

  val tests = Tests {
    test("empty") {
      val params = Nil
      val result = GetOpt.parse(params, Nil)
      result.knownArgs.size ==> 0
      result.unknownArgs.size ==> 0
    }
    test("positional") {
      val params = Seq(
        Param("a"),
        Param("b")
      )
      test("missing") {
        val result = GetOpt.parse(params, Nil)
        result.knownArgs.foreach(kv => assert(kv._2.isEmpty))
        result.unknownArgs.size ==> 0
      }
      test("partial missing") {
        val result = GetOpt.parse(params, List("a"))
        result.knownArgs("a").size ==> 1
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").size ==> 0
        result.unknownArgs.size ==> 0
      }
      test("ok") {
        val result = GetOpt.parse(params, List("a", "b"))
        result.knownArgs.size ==> 2
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").head ==> Arg("b", Some("b"))
        result.unknownArgs.size ==> 0
      }
      test("too many") {
        val result = GetOpt.parse(params, List("a", "b", "c"))
        result.knownArgs.size ==> 2
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").head ==> Arg("b", Some("b"))
        result.unknownArgs.size ==> 1
        result.unknownArgs(0) ==> "c"
      }
      test("too many with named") {
        val result = GetOpt.parse(params, List("a", "b", "--c"))
        result.knownArgs.size ==> 2
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").head ==> Arg("b", Some("b"))
        result.unknownArgs.size ==> 1
        result.unknownArgs(0) ==> "--c"
      }
      test("too many with named out of order") {
        val result = GetOpt.parse(params, List("a", "--c", "b"))
        result.knownArgs.size ==> 2
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").head ==> Arg("b", Some("b"))
        result.unknownArgs.size ==> 1
        result.unknownArgs(0) ==> "--c"
      }
    }
    test("positional repeated") {
      val params = Seq(Param("p1"), Param("p2", positionalRepeats = true))
      test("ok") {
        val result = GetOpt.parse(params, List("a", "b", "c", "d"))
        result.knownArgs.size ==> 2
        result.knownArgs("p1").size ==> 1
        result.knownArgs("p1").head ==> Arg("p1", Some("a"))
        result.knownArgs("p2") ==> ArrayBuffer(
          Arg("p2", Some("b")),
          Arg("p2", Some("c")),
          Arg("p2", Some("d"))
        )
        result.unknownArgs.size ==> 0
      }
      test("ok2") {
        val result = GetOpt.parse(params, List("a", "--b", "c", "d"))
        result.knownArgs("p1") ==> ArrayBuffer(Arg("p1", Some("a")))
        result.knownArgs("p2") ==> ArrayBuffer(
          Arg("p2", Some("c")),
          Arg("p2", Some("d"))
        )
        result.unknownArgs ==> ArrayBuffer("--b")
      }
      test("ok3") {
        val result = GetOpt.parse(params, List("a", "--", "c", "d"))
        result.knownArgs("p1") ==> ArrayBuffer(Arg("p1", Some("a")))
        result.knownArgs("p2") ==> ArrayBuffer(
          Arg("p2", Some("c")),
          Arg("p2", Some("d"))
        )
        result.unknownArgs.size ==> 0
      }
      test("ok4") {
        val result = GetOpt.parse(params, List("a", "--", "c", "--d"))
        result.knownArgs("p1") ==> ArrayBuffer(Arg("p1", Some("a")))
        result.knownArgs("p2") ==> ArrayBuffer(
          Arg("p2", Some("c")),
          Arg("p2", Some("--d"))
        )
        result.unknownArgs.size ==> 0
      }
    }
    test("named args") {
      val params = Seq(
        Param("--p1"),
        Param("--p2")
      )
      test("missing") {
        val result = GetOpt.parse(params, Nil)
        result.knownArgs.foreach(kv => assert(kv._2.isEmpty))
        result.unknownArgs.size ==> 0
      }
      test("partial") {
        val result = GetOpt.parse(params, List("--p1"))
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer()
        )
        result.unknownArgs.size ==> 0
      }
      test("ok") {
        val result = GetOpt.parse(params, List("--p1", "--p2"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        result.unknownArgs.size ==> 0
      }
      test("too many") {
        val result = GetOpt.parse(params, List("--p1", "--p2", "--p3"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        result.unknownArgs.size ==> 1
        result.unknownArgs(0) ==> "--p3"
      }
      test("too many out of order") {
        val result = GetOpt.parse(params, List("--p1", "--p3", "--p2"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        result.unknownArgs.size ==> 1
        result.unknownArgs(0) ==> "--p3"
      }
      test("too many out of order with pos") {
        val result = GetOpt.parse(params, List("p0", "--p1", "--p3", "--p2"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        result.unknownArgs.size ==> 2
        result.unknownArgs(0) ==> "p0"
        result.unknownArgs(1) ==> "--p3"
      }
      test("with arg") {
        val result = GetOpt.parse(params, List("--p1", "p0", "extra", "--p2=--p3", "extra"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", Some("p0"))),
          "--p2" -> ArrayBuffer(Arg("--p2", Some("--p3")))
        )
        result.unknownArgs.size ==> 2
        result.unknownArgs(0) ==> "extra"
        result.unknownArgs(1) ==> "extra"
      }
      test("repeated") {
        val result = GetOpt.parse(params, List("--p1", "p0", "extra", "--p2=--p3", "--p1", "extra", "--p1", "--last"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(
            Arg("--p1", Some("p0")),
            Arg("--p1", Some("extra")),
            Arg("--p1", None)
          ),
          "--p2" -> ArrayBuffer(Arg("--p2", Some("--p3")))
        )
        result.unknownArgs.size ==> 2
        result.unknownArgs(0) ==> "extra"
        result.unknownArgs(1) ==> "--last"
      }
    }
    test("named flag") {
      val params = Seq(
        Param("--p1", flag = true),
        Param("--p2")
      )
      test("missing") {
        val result = GetOpt.parse(params, Nil)
        result.knownArgs.foreach(kv => assert(kv._2.isEmpty))
        result.unknownArgs.size ==> 0
      }
      test("partial") {
        val result = GetOpt.parse(params, List("--p1"))
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer()
        )
        result.unknownArgs.size ==> 0
      }
      test("ok") {
        val result = GetOpt.parse(params, List("--p1", "--p2"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        result.unknownArgs.size ==> 0
      }
      test("too many") {
        val result = GetOpt.parse(params, List("--p1", "--p2", "--p3"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        result.unknownArgs.size ==> 1
        result.unknownArgs(0) ==> "--p3"
      }
      test("too many out of order") {
        val result = GetOpt.parse(params, List("--p1", "--p3", "--p2"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        result.unknownArgs.size ==> 1
        result.unknownArgs(0) ==> "--p3"
      }
      test("too many out of order with pos") {
        val result = GetOpt.parse(params, List("p0", "--p1", "--p3", "--p2"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        result.unknownArgs.size ==> 2
        result.unknownArgs(0) ==> "p0"
        result.unknownArgs(1) ==> "--p3"
      }
      test("with arg") {
        val result = GetOpt.parse(params, List("--p1", "p0", "extra", "--p2=--p3", "extra"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", Some("--p3")))
        )
        result.unknownArgs.size ==> 3
        result.unknownArgs(0) ==> "p0"
        result.unknownArgs(1) ==> "extra"
        result.unknownArgs(2) ==> "extra"
      }
      test("with arg override") {
        val result = GetOpt.parse(params, List("--p1=p0", "extra", "--p2=--p3", "extra"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", Some("p0"))),
          "--p2" -> ArrayBuffer(Arg("--p2", Some("--p3")))
        )
        result.unknownArgs.size ==> 2
        result.unknownArgs(0) ==> "extra"
        result.unknownArgs(1) ==> "extra"
      }
    }
    test("alias") {
      val params = Seq(
        Param("--p1"),
        Param("--p1", aliases = Seq("-p")),
        Param("--p2")
      )
      val result = GetOpt.parse(params, List("-p", "ok", "--p1", "--p2", "--p1=a"))
      result.knownArgs ==> LinkedHashMap(
        "--p1" -> ArrayBuffer(
          Arg("-p", Some("ok")),
          Arg("--p1", None),
          Arg("--p1", Some("a")),
        ),
        "--p2" -> ArrayBuffer(
          Arg("--p2", None)
        )
      )
    }
    test("short multichar") {
      val params = Seq(
        Param("-p1"),
        Param("-p2")
      )
      test("embedded") {
        val result = GetOpt.parse(params, List("-p1=a", "-p2=b"))
        result.knownArgs ==> LinkedHashMap(
          "-p1" -> ArrayBuffer(Arg("-p1", Some("a"))),
          "-p2" -> ArrayBuffer(Arg("-p2", Some("b")))
        )
        result.unknownArgs.size ==> 0
      }
      test("non embedded") {
        val result = GetOpt.parse(params, List("-p1", "a", "-p2", "b", "-p2"))
        result.knownArgs ==> LinkedHashMap(
          "-p1" -> ArrayBuffer(Arg("-p1", Some("a"))),
          "-p2" -> ArrayBuffer(
            Arg("-p2", Some("b")),
            Arg("-p2", None)
          )
        )
        result.unknownArgs.size ==> 0
      }
    }
    test("short flags") {
      val params = Seq(
        Param("-a", flag = true),
        Param("-b", flag = true),
        Param("-c", flag = true)
      )
      test("all") {
        val result = GetOpt.parse(params, List("-abc"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "-c" -> ArrayBuffer(Arg("-c", None))
        )
        result.unknownArgs.size ==> 0
      }
      test("order") {
        val result = GetOpt.parse(params, List("-cab"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "-c" -> ArrayBuffer(Arg("-c", None))
        )
        result.unknownArgs.size ==> 0
      }
      test("partial") {
        val result = GetOpt.parse(params, List("-cb"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "-c" -> ArrayBuffer(Arg("-c", None)),
        )
        result.unknownArgs.size ==> 0
      }
      test("partial2") {
        val result = GetOpt.parse(params, List("-cb", "-a"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "-c" -> ArrayBuffer(Arg("-c", None))
        )
        result.unknownArgs.size ==> 0
      }
    }
    test("options1") {
      val params = Seq(Param("-D"))
      test("no value") {
        val result = GetOpt.parse(params, List("-D"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", None))
        )
        result.unknownArgs.size ==> 0
      }
      test("with value") {
        val result = GetOpt.parse(params, List("-Dhello"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("hello")))
        )
        result.unknownArgs.size ==> 0
      }
      test("with value separate") {
        val result = GetOpt.parse(params, List("-D", "hello"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("hello")))
        )
        result.unknownArgs.size ==> 0
      }
      test("with value embedded") {
        val result = GetOpt.parse(params, List("-D=hello"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("hello")))
        )
        result.unknownArgs.size ==> 0
      }
      test("with value embedded equals") {
        val result = GetOpt.parse(params, List("-D=hello=world"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("hello=world")))
        )
        result.unknownArgs.size ==> 0
      }
      test("with separate equals") {
        val result = GetOpt.parse(params, List("-D", "a=b"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("a=b")))
        )
        result.unknownArgs.size ==> 0
      }
    }
    test("short mixed") {
      val params = Seq(
        Param("-a", flag = true),
        Param("-b"),
        Param("0")
      )
      test("ok1") {
        val result = GetOpt.parse(params, List("-bhello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", Some("hello"))),
          "0" -> ArrayBuffer()
        )
        result.unknownArgs.size ==> 0
      }
      test("ok2") {
        val result = GetOpt.parse(params, List("-bahello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", Some("ahello"))),
          "0" -> ArrayBuffer()
        )
        result.unknownArgs.size ==> 0
      }
      test("ok3") {
        val result = GetOpt.parse(params, List("-b", "hello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", Some("hello"))),
          "0" -> ArrayBuffer()
        )
        result.unknownArgs.size ==> 0
      }
      test("withflag1") {
        val result = GetOpt.parse(params, List("-abhello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", Some("hello"))),
          "0" -> ArrayBuffer()
        )
        result.unknownArgs.size ==> 0
      }
      test("withflag2") {
        val result = GetOpt.parse(params, List("-ab", "hello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "0" -> ArrayBuffer(Arg("0", Some("hello")))
        )
        result.unknownArgs.size ==> 0
      }
      test("withflag2") {
        val result = GetOpt.parse(params, List("-ba", "hello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", Some("a"))),
          "0" -> ArrayBuffer(Arg("0", Some("hello")))
        )
        result.unknownArgs.size ==> 0
      }
    }
  }
