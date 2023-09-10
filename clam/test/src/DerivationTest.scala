import utest._

import clam.Result.Success

object DerivationTest extends TestSuite:

  // /** Hello, world!
  //   * @param cool Something nice
  //   */
  // case class Cli(
  //   // foo: String = "",
  //   @clam.param(aliases = Seq("-b", "--cool-beans")) bar: Int = 3,
  //   cool: Seq[Boolean] = Seq(),
  //   scmd: FooEnum
  // ) derives api.Command
  //   // require(false, "nooo")

  // sealed trait FooEnum2 derives api.Subcommand
  // /** *yes yes!*/
  // case class Yo(x: Int) extends FooEnum2

  // enum FooEnum derives api.Subcommand:
  //   case Yo(a: Int)
  //   case Bar(nested: NestedEnum)

  //   // @subcommand.none()
  //   case None()

  //   // @subcommand.unknown()
  //   case External(name: String, args: Seq[String])


  // enum NestedEnum derives api.Subcommand:
  //   case Cool()

  object custom extends clam.api.Api

  val tests = Tests{
    test("empty"){
      case class Cli() derives clam.Command
      val result = clam.Command[Cli].parse(List())
      assert(result == Success(Cli()))
    }
    test("positional"){
      case class Cli(
        pos1: String,
        pos2: Int
      ) derives clam.Command
      assert(clam.Command[Cli].parse(List("pos1", "2")) == Success(Cli("pos1", 2)))
    }
    test("named"){
      case class Cli(
        param1: String = "",
        param2: Int = 42
      ) derives clam.Command
      assert(clam.Command[Cli].parse(List("--param1", "a", "--param2=1")) == Success(Cli("a", 1)))
      assert(clam.Command[Cli].parse(List("--param1", "a")) == Success(Cli("a", 42)))
      assert(clam.Command[Cli].parse(List("--param2=1")) == Success(Cli("", 1)))
      assert(clam.Command[Cli].parse(List()) == Success(Cli("", 42)))
    }
    test("subcommand") {
      case class Cli(
        param1: String = "",
        param2: Int = 42,
        subcmd: SubCli
      ) derives clam.Command
      assert(clam.Command[Cli].parse(List("--param1", "a", "--param2=1", "foo", "a")) == Success(Cli("a", 1, SubCli.Foo("a"))))
      assert(clam.Command[Cli].parse(List("--param1", "a", "--param2=1", "bar", "2")) == Success(Cli("a", 1, SubCli.Bar(2))))
      assert(clam.Command[Cli].parse(List("--param1", "a", "--param2=1", "baz")) == Success(Cli("a", 1, SubCli.Baz())))
    }
  }

  // Note: there's a bug in the test macro that prevents this from being defined in the macro
  enum SubCli derives clam.Subcommand:
    case Foo(x: String)
    case Bar(y: Int)
    case Baz()

