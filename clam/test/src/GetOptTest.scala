import utest._

import clam.getopt
import clam.getopt.parse
import clam.getopt.Arg
import clam.getopt.Param
import collection.mutable.ArrayBuffer
import collection.mutable.LinkedHashMap

object GetOptTest extends TestSuite:

  val tests = Tests {
    test("empty") {
      val params = Nil
      val result = parse(params, Nil)
      result.knownArgs.size ==> 0
      assert(result.unknown.isEmpty)
    }
    test("positional") {
      val params = Seq(
        Param("a"),
        Param("b")
      )
      test("missing") {
        val result = parse(params, Nil)
        result.knownArgs.foreach(kv => assert(kv._2.isEmpty))
        assert(result.unknown.isEmpty)
      }
      test("partial missing") {
        val result = parse(params, List("a"))
        result.knownArgs("a").size ==> 1
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").size ==> 0
        assert(result.unknown.isEmpty)
      }
      test("ok") {
        val result = parse(params, List("a", "b"))
        result.knownArgs.size ==> 2
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").head ==> Arg("b", Some("b"))
        assert(result.unknown.isEmpty)
      }
      test("too many") {
        val result = parse(params, List("a", "b", "c"))
        result.knownArgs.size ==> 2
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").head ==> Arg("b", Some("b"))

        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "c"
        remainder.hasNext ==> false
      }
      test("too many with named") {
        val result = parse(params, List("a", "b", "--c"))
        result.knownArgs.size ==> 2
        result.knownArgs("a").head ==> Arg("a", Some("a"))
        result.knownArgs("b").head ==> Arg("b", Some("b"))

        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "--c"
        remainder.hasNext ==> false
      }
      test("too many with named out of order") {
        val result = parse(params, List("a", "--c", "b"))
        result.knownArgs("a").head ==> Arg("a", Some("a"))

        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "--c"
        remainder.next() ==> "b"
        remainder.hasNext ==> false
      }
    }
    test("positional repeated") {
      val params = Seq(Param("p1"), Param("p2", positionalRepeats = true))
      test("ok") {
        val result = parse(params, List("a", "b", "c", "d"))
        result.knownArgs.size ==> 2
        result.knownArgs("p1").size ==> 1
        result.knownArgs("p1").head ==> Arg("p1", Some("a"))
        result.knownArgs("p2") ==> ArrayBuffer(
          Arg("p2", Some("b")),
          Arg("p2", Some("c")),
          Arg("p2", Some("d"))
        )
        assert(result.unknown.isEmpty)
      }
      test("ok2") {
        val result = parse(params, List("a", "--b", "c", "d"))
        result.knownArgs("p1") ==> ArrayBuffer(Arg("p1", Some("a")))

        result.unknown.isDefined ==> true
        val (uarg, remainder) = result.unknown.get
        uarg ==> "--b"
        remainder.next() ==> "c"
        remainder.next() ==> "d"
        remainder.hasNext ==> false
      }
      test("ok3") {
        val result = parse(params, List("a", "--", "c", "d"))
        result.knownArgs("p1") ==> ArrayBuffer(Arg("p1", Some("a")))
        result.knownArgs("p2") ==> ArrayBuffer(
          Arg("p2", Some("c")),
          Arg("p2", Some("d"))
        )
        assert(result.unknown.isEmpty)
      }
      test("ok4") {
        val result = parse(params, List("a", "--", "c", "--d"))
        result.knownArgs("p1") ==> ArrayBuffer(Arg("p1", Some("a")))
        result.knownArgs("p2") ==> ArrayBuffer(
          Arg("p2", Some("c")),
          Arg("p2", Some("--d"))
        )
        assert(result.unknown.isEmpty)
      }
    }
    test("named args") {
      val params = Seq(
        Param("--p1"),
        Param("--p2")
      )
      test("missing") {
        val result = parse(params, Nil)
        result.knownArgs.foreach(kv => assert(kv._2.isEmpty))
        assert(result.unknown.isEmpty)
      }
      test("partial") {
        val result = parse(params, List("--p1"))
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer()
        )
        assert(result.unknown.isEmpty)
      }
      test("ok") {
        val result = parse(params, List("--p1", "--p2"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        assert(result.unknown.isEmpty)
      }
      test("too many") {
        val result = parse(params, List("--p1", "--p2", "--p3"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "--p3"
        remainder.hasNext ==> false
      }
      test("too many out of order") {
        val result = parse(params, List("--p1", "--p3", "--p2"))
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer()
        )
        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "--p3"
        remainder.next ==> "--p2"
        remainder.hasNext ==> false
      }
      test("too many out of order with pos") {
        val result = parse(params, List("--p2", "--p1", "--p3", "p0"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p2" -> ArrayBuffer(Arg("--p2", None)),
          "--p1" -> ArrayBuffer(Arg("--p1", None))
        )

        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "--p3"
        remainder.next ==> "p0"
        remainder.hasNext ==> false
      }
      test("with arg") {
        val result = parse(params, List("--p1", "p0", "--p2=--p3", "extra"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", Some("p0"))),
          "--p2" -> ArrayBuffer(Arg("--p2", Some("--p3")))
        )

        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "extra"
        remainder.hasNext ==> false
      }
      test("repeated") {
        val result = parse(params, List("--p1", "p0","--p2=--p3", "--p1", "extra", "--p1", "--last", "extra"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(
            Arg("--p1", Some("p0")),
            Arg("--p1", Some("extra")),
            Arg("--p1", None)
          ),
          "--p2" -> ArrayBuffer(Arg("--p2", Some("--p3")))
        )


        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "--last"
        remainder.next() ==> "extra"
        remainder.hasNext ==> false
      }
    }
    test("named flag") {
      val params = Seq(
        Param("--p1", flag = true),
        Param("--p2")
      )
      test("missing") {
        val result = parse(params, Nil)
        result.knownArgs.foreach(kv => assert(kv._2.isEmpty))
        assert(result.unknown.isEmpty)
      }
      test("partial") {
        val result = parse(params, List("--p1"))
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer()
        )
        assert(result.unknown.isEmpty)
      }
      test("ok") {
        val result = parse(params, List("--p1", "--p2"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        assert(result.unknown.isEmpty)
      }
      test("too many") {
        val result = parse(params, List("--p1", "--p2", "--p3"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer(Arg("--p2", None))
        )
        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "--p3"
        remainder.hasNext ==> false
      }
      test("too many out of order") {
        val result = parse(params, List("--p1", "--p3", "--p2"))
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", None)),
          "--p2" -> ArrayBuffer()
        )
        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "--p3"
        remainder.next() ==> "--p2"
        remainder.hasNext ==> false
      }
      test("too many out of order with pos") {
        val result = parse(params, List( "--p2", "--p1", "p0", "--p3"))
        result.knownArgs ==> LinkedHashMap(
          "--p2" -> ArrayBuffer(Arg("--p2", None)),
          "--p1" -> ArrayBuffer(Arg("--p1", None))
        )

        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "p0"
        remainder.next() ==> "--p3"
        remainder.hasNext ==> false
      }
      test("with arg") {
        val result = parse(params, List("--p2=--p3", "--p1", "p0", "extra", "extra"))
        result.knownArgs ==> LinkedHashMap(
          "--p2" -> ArrayBuffer(Arg("--p2", Some("--p3"))),
          "--p1" -> ArrayBuffer(Arg("--p1", None))
        )

        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "p0"
        remainder.next() ==> "extra"
        remainder.next() ==> "extra"
        remainder.hasNext ==> false
      }
      test("with arg override") {
        val result = parse(params, List("--p1=p0", "--p2=--p3", "extra"))
        result.knownArgs.size ==> 2
        result.knownArgs ==> LinkedHashMap(
          "--p1" -> ArrayBuffer(Arg("--p1", Some("p0"))),
          "--p2" -> ArrayBuffer(Arg("--p2", Some("--p3")))
        )

        assert(result.unknown.isDefined)
        val (unknownArg, remainder) = result.unknown.get
        unknownArg ==> "extra"
        remainder.hasNext ==> false
      }
    }
    test("alias") {
      val params = Seq(
        Param("--p1"),
        Param("--p1", aliases = Seq("-p")),
        Param("--p2")
      )
      val result = parse(params, List("-p", "ok", "--p1", "--p2", "--p1=a"))
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
        val result = parse(params, List("-p1=a", "-p2=b"))
        result.knownArgs ==> LinkedHashMap(
          "-p1" -> ArrayBuffer(Arg("-p1", Some("a"))),
          "-p2" -> ArrayBuffer(Arg("-p2", Some("b")))
        )
        assert(result.unknown.isEmpty)
      }
      test("non embedded") {
        val result = parse(params, List("-p1", "a", "-p2", "b", "-p2"))
        result.knownArgs ==> LinkedHashMap(
          "-p1" -> ArrayBuffer(Arg("-p1", Some("a"))),
          "-p2" -> ArrayBuffer(
            Arg("-p2", Some("b")),
            Arg("-p2", None)
          )
        )
        assert(result.unknown.isEmpty)
      }
    }
    test("short flags") {
      val params = Seq(
        Param("-a", flag = true),
        Param("-b", flag = true),
        Param("-c", flag = true)
      )
      test("all") {
        val result = parse(params, List("-abc"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "-c" -> ArrayBuffer(Arg("-c", None))
        )
        assert(result.unknown.isEmpty)
      }
      test("order") {
        val result = parse(params, List("-cab"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "-c" -> ArrayBuffer(Arg("-c", None))
        )
        assert(result.unknown.isEmpty)
      }
      test("partial") {
        val result = parse(params, List("-cb"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "-c" -> ArrayBuffer(Arg("-c", None)),
        )
        assert(result.unknown.isEmpty)
      }
      test("partial2") {
        val result = parse(params, List("-cb", "-a"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "-c" -> ArrayBuffer(Arg("-c", None))
        )
        assert(result.unknown.isEmpty)
      }
    }
    test("options1") {
      val params = Seq(Param("-D"))
      test("no value") {
        val result = parse(params, List("-D"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", None))
        )
        assert(result.unknown.isEmpty)
      }
      test("with value") {
        val result = parse(params, List("-Dhello"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("hello")))
        )
        assert(result.unknown.isEmpty)
      }
      test("with value separate") {
        val result = parse(params, List("-D", "hello"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("hello")))
        )
        assert(result.unknown.isEmpty)
      }
      test("with value embedded") {
        val result = parse(params, List("-D=hello"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("hello")))
        )
        assert(result.unknown.isEmpty)
      }
      test("with value embedded equals") {
        val result = parse(params, List("-D=hello=world"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("hello=world")))
        )
        assert(result.unknown.isEmpty)
      }
      test("with separate equals") {
        val result = parse(params, List("-D", "a=b"))
        result.knownArgs ==> LinkedHashMap(
          "-D" -> ArrayBuffer(Arg("-D", Some("a=b")))
        )
        assert(result.unknown.isEmpty)
      }
    }
    test("short mixed") {
      val params = Seq(
        Param("-a", flag = true),
        Param("-b"),
        Param("0")
      )
      test("ok1") {
        val result = parse(params, List("-bhello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", Some("hello"))),
          "0" -> ArrayBuffer()
        )
        assert(result.unknown.isEmpty)
      }
      test("ok2") {
        val result = parse(params, List("-bahello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", Some("ahello"))),
          "0" -> ArrayBuffer()
        )
        assert(result.unknown.isEmpty)
      }
      test("ok3") {
        val result = parse(params, List("-b", "hello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", Some("hello"))),
          "0" -> ArrayBuffer()
        )
        assert(result.unknown.isEmpty)
      }
      test("withflag1") {
        val result = parse(params, List("-abhello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", Some("hello"))),
          "0" -> ArrayBuffer()
        )
        assert(result.unknown.isEmpty)
      }
      test("withflag2") {
        val result = parse(params, List("-ab", "hello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(Arg("-a", None)),
          "-b" -> ArrayBuffer(Arg("-b", None)),
          "0" -> ArrayBuffer(Arg("0", Some("hello")))
        )
        assert(result.unknown.isEmpty)
      }
      test("withflag2") {
        val result = parse(params, List("-ba", "hello"))
        result.knownArgs ==> LinkedHashMap(
          "-a" -> ArrayBuffer(),
          "-b" -> ArrayBuffer(Arg("-b", Some("a"))),
          "0" -> ArrayBuffer(Arg("0", Some("hello")))
        )
        assert(result.unknown.isEmpty)
      }
    }
  }
