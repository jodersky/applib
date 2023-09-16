import utest._

import clam.Result.Success

object DerivationTestDefault extends TestSuite:

  val tests = Tests{
    test("empty"){
      case class Cli() derives clam.Command
      val result = clam.parse[Cli](List())
      assert(result == Success(Cli()))
    }
    test("positional"){
      case class Cli(
        pos1: String,
        pos2: Int
      ) derives clam.Command
      assert(clam.parse[Cli](List("pos1", "2")) == Success(Cli("pos1", 2)))
    }
    test("named"){
      case class Cli(
        param1: String = "",
        param2: Int = 42
      ) derives clam.Command
      assert(clam.parse[Cli](List("--param1", "a", "--param2=1")) == Success(Cli("a", 1)))
      assert(clam.parse[Cli](List("--param1", "a")) == Success(Cli("a", 42)))
      assert(clam.parse[Cli](List("--param2=1")) == Success(Cli("", 1)))
      assert(clam.parse[Cli](List()) == Success(Cli("", 42)))
    }
    test("subcommand") {
      case class Cli(
        param1: String = "",
        param2: Int = 42,
        subcmd: SubCli
      ) derives clam.Command
      assert(clam.parse[Cli](List("--param1", "a", "--param2=1", "foo", "a")) == Success(Cli("a", 1, SubCli.Foo("a"))))
      assert(clam.parse[Cli](List("--param1", "a", "--param2=1", "bar", "2")) == Success(Cli("a", 1, SubCli.Bar(2))))
      assert(clam.parse[Cli](List("--param1", "a", "--param2=1", "baz")) == Success(Cli("a", 1, SubCli.Baz())))
    }
  }

  // Note: there's a bug in the test macro that prevents this from being defined in the macro
  enum SubCli derives clam.Subcommand:
    case Foo(x: String)
    case Bar(y: Int)
    case Baz()

