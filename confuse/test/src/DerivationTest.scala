package test

import utest._

import confuse.parsers.Result.Success
import confuse.parsers.Result.FieldErrors
import confuse.parsers.FieldError

object DerivationTest extends TestSuite {
  def tests: Tests = Tests{
    test("basic") {
      case class Inner(bar: Int = 42) derives confuse.default.Parser
      case class MyConfig(fooBar: Int = 2, inner: Inner) derives confuse.default.Parser

      test("defaults") {
        confuse.read().parse[MyConfig]() ==> Success(MyConfig(2, Inner(42)))
      }
      test("override") {
        confuse.read(args = Map("foo_bar" -> "1", "inner.bar" -> "2")).parse[MyConfig]() ==>
          Success(MyConfig(1, Inner(2)))
      }
    }
    test("missing") {
      case class Inner(bar: Int) derives confuse.default.Parser
      case class MyConfig(fooBar: Int, inner: Inner) derives confuse.default.Parser

      test("defaults") {
        val errs = confuse.read().parse[MyConfig]().asInstanceOf[FieldErrors].errs
        errs ==> Seq(
          FieldError.TypeMismatch("foo_bar", confuse.Null(), "integer"),
          FieldError.TypeMismatch("inner.bar", confuse.Null(), "integer")
        )
      }
      test("partial1") {
        val errs = confuse.read(args=Map("foo_bar" -> "1")).parse[MyConfig]().asInstanceOf[FieldErrors].errs
        errs ==> Seq(
          FieldError.TypeMismatch("inner.bar", confuse.Null(), "integer")
        )
      }
      test("partial2") {
        val errs = confuse.read(args=Map("inner.bar" -> "0")).parse[MyConfig]().asInstanceOf[FieldErrors].errs
        errs ==> Seq(
          FieldError.TypeMismatch("foo_bar", confuse.Null(), "integer")
        )
      }
      test("mix") {
        val errs = confuse.read(args=Map("inner.bar" -> "a")).parse[MyConfig]().asInstanceOf[FieldErrors].errs
        errs ==> Seq(
          FieldError.TypeMismatch("foo_bar", confuse.Null(), "integer"),
          FieldError.TypeMismatch("inner.bar", confuse.Str("a"), "integer")
        )
      }
      test("ok") {
        confuse.read(args=Map("inner.bar" -> "1", "foo_bar" -> "2")).parse[MyConfig]() ==>
          Success(
            MyConfig(2, Inner(1))
          )
      }
    }
  }

}
