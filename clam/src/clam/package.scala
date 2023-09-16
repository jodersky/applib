package clam

import clam.util.Terminal
import clam.derivation.Reporter

// Make the default readers and derivation API available as top-level `clam.*`
// calls.
export clam.derivation.DefaultApi.*
export clam.derivation.Result
export clam.derivation.Reporter
export clam.derivation.param

def parse[A](
  args: Iterable[String],
  reporter: Reporter = Reporter(System.out, System.err),
  terminal: Terminal = Terminal.current()
)(using cmd: clam.derivation.DerivationApi#Command[A]): Result[A] =
  val ctx = clam.derivation.ParseCtx(reporter, terminal)
  cmd.extract(
    getopt.parse(
      cmd.parsers.flatMap(_.paramDefs).map(_.parseInfo),
      args.iterator
    ),
    ctx
  )

def parseOrExit[A](
  args: Iterable[String],
  reporter: Reporter = Reporter(System.out, System.err),
  terminal: Terminal = Terminal.current(),
  exit: Int => Nothing = sys.exit
)(using clam.derivation.DerivationApi#Command[A]): A =
  parse(args, reporter, terminal) match
    case Result.Success(a) => a
    case Result.ArgumentError => exit(2)
    case Result.EarlyExit => exit(0)
