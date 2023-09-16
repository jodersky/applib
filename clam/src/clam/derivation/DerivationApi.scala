package clam.derivation

import clam.readers
import clam.getopt
import clam.core
import clam.text
import clam.util.Terminal
import clam.completion


trait DerivationApi
  extends readers.ReaderApi
  with SupportApi
  with CompletionApi
  with LowPrioCompleters: // make sure we don't fail to derive a parser if a completer is not defined
  api =>
  import collection.mutable as m

  /** A parser is responsible for defining a set of CLI parameters and
    * implementing a way to extract a value from the corresponding arguments.
    */
  trait Parser[+A]:
    def paramDefs: Vector[ParamDef]
    def extract(args: getopt.Result, ctx: ParseCtx): Result[A]

    /** Set this to true if the extraction will handle an unknown getopt result
      * and its remainder. */
    def acceptsUnknown: Boolean = false

    /** Override this to define subcommands.
      *
      * Notes:
      * -  There can only be one parser which defines subcommands, and this
      *    parser must come last. More than one will lead to an impossible CLI
      *    grammar.
      * - The reason for defining subcommands in the Parser trait (instead of
      *   subclassing it), is so that parsers can easily be composed and reused.
      */
    def subcommands: Map[String, Command[?]] = Map.empty

  /** A single CLI parameter (may not be repeated) which maps to a single scala parameter. */
  case class SingleParam[A](
    scalaName: String,
    default: Option[() => A],
    annot: param,
    doc: String,
    argName0: Option[String],
    reader0: Reader[A],
    completer: Completer[A]
  ) extends Parser[A]:

    val name = annot.name match
      case null if default.isDefined => scalaToNamedParam(scalaName)
      case null  => scalaToPositionalParam(scalaName)
      case n => n

    val reader = annot.reader match
      case null => reader0
      case r => r.asInstanceOf[Reader[A]]

    val argName = annot.argName match
      case null => argName0
      case n => Some(n)

    val pdef = ParamDef(
      names = Seq(name) ++ annot.aliases,
      argName = argName,
      repeats = false,
      description = doc,
      endOfNamed = annot.endOfNamed,
      interactiveCompleter = annot.interactiveCompleter match
        case null => completer.interactiveCompleter
        case x => x,
      standaloneCompleter = annot.standaloneCompleter match
        case null => completer.standaloneCompleter
        case x => x
    )
    val paramDefs = Vector(pdef)

    def extract(args: getopt.Result, ctx: ParseCtx): Result[A] =
      import ctx.reporter
      args.knownArgs(name).lastOption match
        case Some(arg) => arg.arg match
          case Some(value) =>
            reader.read(value) match
              case Reader.Result.Success(a) =>
                Result.Success(a)
              case Reader.Result.Error(message) =>
                reporter.error(s"error parsing argument ${arg.nameUsed} ($value): $message")
                Result.ArgumentError
          case None if !argName.isDefined =>
            reader.read("true") match
              case Reader.Result.Success(a) =>
                Result.Success(a)
              case Reader.Result.Error(message) =>
                reporter.error(s"error parsing argument ${arg.nameUsed}: $message")
                Result.ArgumentError
          case None =>
            reporter.error(s"error parsing argument ${arg.nameUsed}: incomplete, expected argument")
            Result.ArgumentError
        case None if default.isDefined =>
          Result.Success(default.get())
        case None =>
          reporter.error(s"missing argument: $name")
          Result.ArgumentError
  end SingleParam

  /** A single CLI parameter (may be repeated) which maps to a single scala parameter. */
  case class RepeatedParam[A, Col[_] <: Iterable[_]](
    scalaName: String,
    default: Option[() => Col[A]],
    annot: param,
    doc: String,
    argName0: Option[String],
    reader0: Reader[A],
    completer: Completer[A],
    factory: collection.Factory[A, Col[A]],
  ) extends Parser[Col[A]]:

    val name = annot.name match
      case null if default.isDefined => scalaToNamedParam(scalaName)
      case null  => scalaToPositionalParam(scalaName)
      case n => n

    val reader = annot.reader match
      case null => reader0
      case r => r.asInstanceOf[Reader[A]]

    val argName = annot.argName match
      case null => argName0
      case n => Some(n)

    val pdef = ParamDef(
      names = Seq(name) ++ annot.aliases,
      argName = argName,
      repeats = true,
      description = doc,
      endOfNamed = annot.endOfNamed,
  // given explicitParam[A](using p: Subcommand[A]): ScalaParam[A] = (_, _, _, _) => p
  // given explicitParam[A](using p: Subcommand[A]): ScalaParam[A] = (_, _, _, _) => p
      interactiveCompleter = annot.interactiveCompleter match
        case null => completer.interactiveCompleter
        case x => x,
      standaloneCompleter = annot.standaloneCompleter match
        case null => completer.standaloneCompleter
        case x => x
    )
    val paramDefs = Vector(pdef)

    def extract(result: getopt.Result, ctx: ParseCtx): Result[Col[A]] =
      import ctx.reporter
      val args = result.knownArgs(name)
      var i = 0
      var res = factory.newBuilder
      while i < args.length do
        val arg = args(i)
        arg.arg match
          case Some(value) =>
            reader.read(value) match
              case Reader.Result.Success(v) =>
                res += v
              case Reader.Result.Error(m) =>
                reporter.error(s"error parsing argument ${arg.nameUsed} ($value): $m")
                return Result.ArgumentError
          case None if argName.isEmpty =>
            reader.read("true") match
              case Reader.Result.Success(v) =>
                res += v
              case Reader.Result.Error(m) =>
                reporter.error(s"error parsing argument ${arg.nameUsed}: $m")
                return Result.ArgumentError
          case None =>
            reporter.error(s"error parsing argument ${arg.nameUsed}: incomplete, expected argument")
            return Result.ArgumentError
        i += 1
      Result.Success(res.result())
  end RepeatedParam

  // case class Group[A](
  //   parsers: IndexedSeq[Parser[?]],
  //   subcommands: Map[String, Command[?]],
  //   instantiate: IndexedSeq[?] => A,
  // ) extends Parser[A]:
  //   // def paramDefs = parsers.flatMap(_.paramDefs)
  //   def extract(result: getopt.Result, reporter: Reporter, terminal: Terminal): Result[A] = ???

  case class Subcommand[+A](override val subcommands: Map[String, Command[A]]) extends Parser[A]:
    def paramDefs = Vector.empty
    override def acceptsUnknown = true
    def extract(args: getopt.Result, ctx: ParseCtx): Result[A] =
      args.unknown match
        case None =>
          ctx.reporter.error("expected command")
          Result.ArgumentError
        case Some((cmd, args)) if subcommands.contains(cmd) =>
          val scmd = subcommands(cmd)
          val childArgs = getopt.parse(
            scmd.parsers.flatMap(_.paramDefs).map(_.parseInfo),
            args
          )
          subcommands(cmd).extract(childArgs, ctx)
        case Some((cmd, _)) =>
          ctx.reporter.error(s"unknown command '$cmd'")

          val similar = clam.util.Levenshtein.closest(cmd, subcommands.keySet)
          if similar.size > 0 then
            if similar.size == 1 then
              ctx.reporter.stderr.println("The most similar command is")
            else
              ctx.reporter.stderr.println("The most similar commands are")

            for s <- similar.take(3) do
              ctx.reporter.stderr.print("  ")
              ctx.reporter.stderr.println(s)
          Result.ArgumentError


  object Subcommand:
    inline def derived[A]: Subcommand[A] = derivedImpl[A].asInstanceOf[Subcommand[A]]
    private inline def derivedImpl[A] = ${
      macros.deriveSubcommand[api.type, A]('api)
    }

  case class Command[+A](
    parsers: IndexedSeq[Parser[?]],
    instantiate: IndexedSeq[?] => A,
    name: String, // the canonical command name (note that it may be aliased)
    doc: String
  ):
    private val acceptsUnknown = parsers.exists(_.acceptsUnknown)

    private val subcommands = parsers.flatMap(_.subcommands)
    private val paramDefs = parsers.flatMap(_.paramDefs)

    // def printCompletion(commandChain: Seq[String]): Unit =
    //   for (name, cmd) <- subcommands do
    //     cmd.printCommandCompletion(commandChain :+ name)

    //   clam.completion.StandaloneBashCompletion.printCommandCompletion(
    //     commandChain,
    //     parsers

    private def hint(ctx: ParseCtx) =
      ctx.reporter.stderr.println(s"See '${ctx.commandChain.mkString(" ")} --help'.")

    def extract(result: getopt.Result, ctx: ParseCtx): Result[A] =
      ctx.commandChain += name
      result.unknown match
        case Some(("--help", _)) =>
          ctx.reporter.stdout.println(
            defaultHelpMessage(
              ctx.terminal,
              doc,
              ctx.commandChain,
              paramDefs,
              subcommands.toMap.map((k, v) => k -> v.doc)
            )
          )
          return Result.EarlyExit

        case Some(("--bash-completion", _)) =>
          return Result.EarlyExit

        case Some((arg, _)) if !acceptsUnknown =>
          ctx.reporter.error(s"unknown argument: ${result.unknown.get._1}")
          hint(ctx)
          return Result.ArgumentError
        case _ =>

      val fields = new Array[Any](parsers.size)
      var hasErrors: Boolean = false
      var i = 0
      while i < fields.size do
        parsers(i).extract(result, ctx) match
          case Result.Success(value) => fields(i) = value
          case Result.ArgumentError => hasErrors = true
          case Result.EarlyExit => return Result.EarlyExit
        i += 1

      if hasErrors then
        hint(ctx)
        return Result.ArgumentError

      try
        Result.Success(instantiate(collection.immutable.ArraySeq.unsafeWrapArray(fields)))
      catch
        case ex: IllegalArgumentException =>
          ctx.reporter.error(ex.getMessage)
          Result.ArgumentError
  end Command

  object Command:

    inline def derived[A]: Command[A] = derivedImpl[A].asInstanceOf[Command[A]]
    private inline def derivedImpl[A] = ${
      macros.deriveCommand[api.type, A]('api)
    }

  /** Signature of a Scala parameter. These are used by the derivation macros to
    * to instantiate parsers for every parameter. Note that this API should be
    * considered experimental. In the future, this typeclass may be eliminated
    * and the behavior folded directly into macros.
    */
  trait ScalaParam[A]:

    /** Create a parser for a given scala parameter (e.g. method, class
      * constructor). */
    def makeParser(scalaName: String, default: Option[() => A], annot: param, doc: String): Parser[A]

  def scalaToNamedParam(scalaName: String): String =
    val singular = if scalaName.endsWith("s") then scalaName.init else scalaName
    s"--${text.kebabify(singular)}"

  def scalaToPositionalParam(scalaName: String): String =
    val singular = if scalaName.endsWith("s") then scalaName.init else scalaName
    text.kebabify(singular)

  def scalaToSubcommand(scalaName: String): String = text.kebabify(scalaName).toLowerCase()

  given simpleParam[A](using reader: Reader[A], completer: Completer[A]): ScalaParam[A] = (scalaName, default, annot, doc) =>
    SingleParam[A](scalaName, default, annot, doc, Some(reader.typeName), reader, completer)
  given flagParam(using reader: Reader[Boolean], completer: Completer[Boolean]): ScalaParam[Boolean] = (scalaName, default, annot, doc) =>
    SingleParam[Boolean](scalaName, default, annot, doc, None, reader, completer)
  given repeatedParam[A, Col[_] <: Iterable[_]](using reader: Reader[A], completer: Completer[A], factory: collection.Factory[A, Col[A]]): ScalaParam[Col[A]] = (scalaName, default, annot, doc) =>
    RepeatedParam[A, Col](scalaName, default, annot, doc, Some(reader.typeName), reader, completer, factory)
  given repeatedFlagParam[Col[_] <: Iterable[_]](using reader: Reader[Boolean], completer: Completer[Boolean], factory: collection.Factory[Boolean, Col[Boolean]]): ScalaParam[Col[Boolean]] = (scalaName, default, annot, doc) =>
    RepeatedParam[Boolean, Col](scalaName, default, annot, doc, None, reader, completer, factory)

  given explicitParam[A](using p: Parser[A]): ScalaParam[A] = (_, _, _, _) => p

