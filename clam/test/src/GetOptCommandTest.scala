import utest._

import clam.GetOpt
import clam.GetOpt.Command
import clam.GetOpt.Param
import clam.GetOpt.Arg
import collection.mutable.LinkedHashMap
import collection.mutable.ArrayBuffer

object GetOptCommandTest extends TestSuite:

  val tests = Tests {
    test("basic") {
      val top = GetOpt.Command()
      val child = GetOpt.Command()
      top.subcommands += "foo" -> child

      test("no command") {
        val res = GetOpt.parse(top, List())
        res.subcommand ==> None
      }
      test("known command") {
        val res = GetOpt.parse(top, List("foo"))
        assert(res.subcommand.isDefined)
        assert(res.subcommand.get.name == "foo")
      }
      test("unknown command") {
        val res = GetOpt.parse(top, List("bar"))
        assert(res.subcommand.isDefined)
        assert(res.subcommand.get.name == "bar")
      }
    }
    test("with positional") {
      val top = GetOpt.Command()
      top.params += GetOpt.Param("count")
      val child = GetOpt.Command()
      top.subcommands += "foo" -> child

      test("no param") {
        val res = GetOpt.parse(top, List())
        res.subcommand ==> None
        res.knownArgs("count").size ==> 0
      }
      test("param no command") {
        val res = GetOpt.parse(top, List("foo"))
        res.subcommand ==> None
        res.knownArgs ==> LinkedHashMap("count" -> ArrayBuffer(Arg("count", Some("foo"))))
      }
      test("param and command") {
        val res = GetOpt.parse(top, List("foo", "foo"))
        res.knownArgs ==> LinkedHashMap("count" -> ArrayBuffer(Arg("count", Some("foo"))))
        assert(res.subcommand.isDefined)
        res.subcommand.get.args ==> GetOpt.Result(
          LinkedHashMap(),
          ArrayBuffer(),
          None
        )
      }
      test("param and command extra") {
        val res = GetOpt.parse(top, List("foo", "foo", "bar"))
        res.knownArgs ==> LinkedHashMap("count" -> ArrayBuffer(Arg("count", Some("foo"))))
        assert(res.subcommand.isDefined)
        res.subcommand.get.args ==> GetOpt.Result(
          LinkedHashMap(),
          ArrayBuffer("bar"),
          None
        )
      }
      test("unknown command") {
        val res = GetOpt.parse(top, List("foo", "bar", "extra"))
        assert(res.subcommand.isDefined)
        assert(res.subcommand.get.name == "bar")
        res.subcommand.get.args ==> GetOpt.Result(
          LinkedHashMap(),
          ArrayBuffer("extra"),
          None
        )
      }
    }
  }
