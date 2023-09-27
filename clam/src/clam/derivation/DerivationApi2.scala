package clam.derivation

import clam.readers
import clam.getopt
import clam.core
import clam.text
import clam.util.Terminal
import clam.completion

case class Ctx[A](
  parent: A,
  reporter: Reporter,
  terminal: Terminal,
  commandChain: collection.mutable.ListBuffer[String] = collection.mutable.ListBuffer.empty
)


trait DerivationApi2
  extends readers.ReaderApi
  with SupportApi
  with CompletionApi
  with LowPrioCompleters: // make sure we don't fail to derive a parser if a completer is not defined
  api =>
  import collection.mutable as m

  trait Parser[A]:
    def pdefs: Vector[getopt.Param]
    def extract(opts: getopt.Result): Result[A]


  class Subcommand[Parent, Value](using parser: Parser[Value])
      extends ((Ctx[Parent], Iterator[String]) => Result[Any]):

    def action(fn: (Ctx[Parent], Value) => Any): this.type =
      this

    def subcommand(fn: (Ctx[Parent], Value) => Any): this.type =
      this

    def system(): this.type =

      ???

    def apply(ctx: Ctx[Parent], args: Iterator[String]): Result[Any] =
      ???


  class Command[Value](using parser: Parser[Value]) extends Subcommand[Unit, Value]:
    ()
