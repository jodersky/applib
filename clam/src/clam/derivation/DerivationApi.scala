package clam.derivation

import clam.readers

import clam.dispatch.Command
import clam.dispatch.Context
import clam.dispatch.ParamDef
import clam.dispatch.Result
import clam.getopt
import clam.text

case class Def[+A](
  pdefs: Seq[ParamDef],
  parse: (Command, Context, getopt.Result) => Result[A]
)

trait DerivationApi
    extends readers.ReaderApi
    with CompletionApi
    with LowPrioCompleters: // never fail if a completer can't be found
  api =>

  /** Annotate a method for which to generate CLI mappings. */
  class command() extends annotation.StaticAnnotation

  trait Parser[A]:
    def make(
      nameChain: Seq[String], // foo.bar.baz
      default: Option[() => A], //
      annot: param,
      doc: String // doc comment
    ): Def[A]

  case class PrimitiveParser[A](
    isFlag: Boolean,
    reader: Reader[A],
    completer: Completer[A]
  ) extends Parser[A]:

    override def make(nameChain: Seq[String], default: Option[() => A], annot: param, doc: String): Def[A] =
      val name = annot.name match
        case null if default.isDefined =>
          s"--${text.kebabify(nameChain.mkString("."))}".toLowerCase
        case null =>
          text.kebabify(nameChain.mkString(".")).toLowerCase
        case name => name

      val pdef = ParamDef(
        names = Seq(name) ++ annot.aliases,
        description = doc,
        argName = annot.argName match
          case null if isFlag => None
          case null => Some(reader.typeName)
          case name => Some(name),
        repeats = false,
        endOfNamed = annot.endOfNamed,
        interactiveCompleter = annot.interactiveCompleter match
          case null => completer.interactiveCompleter
          case fn => fn,
        standaloneCompleter = annot.standaloneCompleter match
          case null => completer.standaloneCompleter
          case fn => fn
      )

      val parse = (cmd: Command, ctx: Context, args: getopt.Result) =>
        args.knownArgs(name).lastOption match
          case None if default.isDefined =>
            Result.Success(default.get())
          case None =>
            ctx.stderr.println("missing required argument " + name)
            Result.ArgumentError()
          case Some(arg) =>
            def read(str: String) = reader.read(str) match
              case Reader.Result.Error(msg) =>
                ctx.stderr.println(s"error parsing ${arg.nameUsed}: $msg")
                Result.ArgumentError()
              case Reader.Result.Success(a) => Result.Success(a)

            arg.arg match
              case Some(s) => read(s)
              case None if isFlag => read("true")
              case None =>
                ctx.stderr.println(s"argument expected for ${arg.nameUsed}")
                Result.ArgumentError()
      Def(Seq(pdef), parse)
  end PrimitiveParser

  case class RepeatedPrimitiveParser[A, Col[_] <: Iterable[_]](
    isFlag: Boolean,
    reader: Reader[A],
    completer: Completer[A],
    factory: collection.Factory[A, Col[A]]
  ) extends Parser[Col[A]]:

    override def make(nameChain: Seq[String], default: Option[() => Col[A]], annot: param, doc: String): Def[Col[A]] =
      val name = annot.name match
        case null if default.isDefined =>
          s"--${text.kebabify(nameChain.mkString("."))}".toLowerCase
        case null =>
          text.kebabify(nameChain.mkString(".")).toLowerCase
        case name => name

      val pdef = ParamDef(
        names = Seq(name) ++ annot.aliases,
        description = doc,
        argName = annot.argName match
          case null if isFlag => None
          case null => Some(reader.typeName)
          case name => Some(name),
        repeats = true,
        endOfNamed = annot.endOfNamed,
        interactiveCompleter = annot.interactiveCompleter match
          case null => completer.interactiveCompleter
          case fn => fn,
        standaloneCompleter = annot.standaloneCompleter match
          case null => completer.standaloneCompleter
          case fn => fn
      )

      val parse = (cmd: Command, ctx: Context, args: getopt.Result) =>
        var hasError = false
        val builder = factory.newBuilder
        val occcurences = args.knownArgs(name)

        for arg <- occcurences do
          def read(str: String) = reader.read(str) match
            case Reader.Result.Error(msg) =>
              ctx.stderr.println(s"error parsing ${arg.nameUsed}: $msg")
              hasError = true
            case Reader.Result.Success(a) =>
              builder += a

          arg.arg match
            case None if isFlag => read("true")
            case None =>
              ctx.stderr.println(s"argument expected for ${arg.nameUsed}")
              hasError = true
            case Some(str) => read(str)
        end for

        if hasError then Result.ArgumentError()
        else Result.Success(builder.result())

      Def(Seq(pdef), parse)
  end RepeatedPrimitiveParser

  inline given booleanParser(using reader: Reader[Boolean], completer: Completer[Boolean]): Parser[Boolean] = PrimitiveParser[Boolean](true, reader, completer)
  inline given primitiveParser[A](using reader: Reader[A], completer: Completer[A]): Parser[A] = PrimitiveParser[A](false, reader, completer)

  inline given repeatedPrimitiveParser[A, Col[_] <: Iterable[_]](using reader: Reader[A], completer: Completer[A], factory: collection.Factory[A, Col[A]]): Parser[Col[A]] =
    RepeatedPrimitiveParser[A, Col](false, reader, completer, factory)

  inline given repeatedPrimitiveParser[Col[_] <: Iterable[_]](using reader: Reader[Boolean], completer: Completer[Boolean], factory: collection.Factory[Boolean, Col[Boolean]]): Parser[Col[Boolean]] =
    RepeatedPrimitiveParser[Boolean, Col](true, reader, completer, factory)

  def extraParams: Seq[ParamDef] = Seq(
    ParamDef(
      names = Seq("--help"),
      description = "show help message and exit",
      argName = None
    )
  )
  def parse(pdefs: Seq[Def[?]])(command: Command, ctx: Context, args: getopt.Result): Result[Seq[?]] =
    if args.knownArgs("--help").nonEmpty then
      command.printHelp(ctx)
      Result.EarlyExit()
    else
      val values = new Array[Any](pdefs.length)
      var isEarlyExit = false
      var hasErrors = false
      var i = 0
      while i < pdefs.length && !isEarlyExit do
        pdefs(i).parse(command, ctx, args) match
          case Result.EarlyExit() => isEarlyExit = true
          case Result.Success(a) => values(i) = a
          case _ => hasErrors = true
        i += 1

      if isEarlyExit then Result.EarlyExit()
      else if hasErrors then Result.ArgumentError()
      else Result.Success(values.toSeq)
