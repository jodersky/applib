package clam.api

import clam.{Reader => _, *}

case class ParseContext(
  terminal: Terminal
)

trait DerivationApi extends ReaderApi with SupportApi:
  api =>
  import collection.mutable as m

  trait Parser[+A]:
    def paramDefs: Vector[GetOpt.Param]
    def paramInfos: Vector[ParamInfo]
    def subcommands: Map[String, Command[_]]

    def extract(
      ctx: ParseContext,
      reporter: Reporter,
      args: GetOpt.Result
    ): Result[A]

  // /** A scala parameter. Defines one or more CLI params and a way to read them
  //   * as a scala type.
  //   */
  // case class Params[+A](
  //   fieldParsers: IndexedSeq[Parser[_]],
  //   instantiate: IndexedSeq[_] => A
  // ) extends Parser[A]:
  //   def paramDefs: Vector[GetOpt.Param] = fieldParsers.flatMap(_.paramDefs).toVector
  //   def paramInfos: Vector[ParamInfo] = fieldParsers.flatMap(_.paramInfos).toVector
  //   def subcommands: Map[String, Command[_]] = fieldParsers.flatMap(_.subcommands).toMap
  //   def extract(args: GetOpt.Result): Result[A] = ??? // TODO: this could be refined to not permit EarlyExit

  case class SingleParam[A](
    name: String,
    default: Option[() => A],
    flag: Boolean,
    aliases: Seq[String],
    endOfNamed: Boolean,
    reader: Reader[A],
    doc: String,
    interactiveCompleter: String => Seq[String],
    standaloneCompleter: BashCompleter
  ) extends Parser[A]:
    val subcommands: Map[String, Command[?]] = Map.empty

    private val pdef = GetOpt.Param(
      name = name,
      aliases = aliases,
      flag = flag,
      positionalRepeats = false,
      endOfNamed = endOfNamed
    )

    private val pinfo = ParamInfo(
      isNamed = pdef.isNamed,
      names = Seq(pdef.name) ++ pdef.aliases,
      argName = if flag then None else Some("value"),
      repeats = false,
      description = doc,
      interactiveCompleter = interactiveCompleter,
      standaloneCompleter = standaloneCompleter
    )

    val paramDefs = Vector(pdef)
    val paramInfos = Vector(pinfo)

    def extract(ctx: ParseContext, reporter: Reporter, result: GetOpt.Result): Result[A] =
      result.knownArgs(name).lastOption match
        case Some(arg) => arg.arg match
          case Some(value) =>
            reader.read(value) match
              case Reader.Result.Success(a) =>
                Result.Success(a)
              case Reader.Result.Error(message) =>
                reporter.error(s"error parsing argument ${arg.nameUsed} ($value): $message")
                Result.ArgumentError
          case None if flag =>
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

  case class RepeatedParam[A, Col[_] <: Iterable[_]](
    name: String,
    flag: Boolean,
    aliases: Seq[String],
    endOfNamed: Boolean,
    reader: Reader[A],
    factory: collection.Factory[A, Col[A]],
    doc: String,
    interactiveCompleter: String => Seq[String],
    standaloneCompleter: BashCompleter
  ) extends Parser[Col[A]]:
    val subcommands: Map[String, Command[?]] = Map.empty
    private val pdef = GetOpt.Param(
      name = name,
      aliases = aliases,
      flag = flag,
      positionalRepeats = true,
      endOfNamed = endOfNamed
    )
    private val pinfo = ParamInfo(
      isNamed = pdef.isNamed,
      names = Seq(pdef.name) ++ pdef.aliases,
      argName = if flag then None else Some("value"),
      repeats = true,
      description = doc,
      interactiveCompleter = interactiveCompleter,
      standaloneCompleter = standaloneCompleter
    )
    val paramDefs = Vector(pdef)
    val paramInfos = Vector(pinfo)

    def extract(ctx: ParseContext, reporter: Reporter, result: GetOpt.Result): Result[Col[A]] =
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
          case None if flag =>
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

  case class Subcommand[+A](
    val subcommands: Map[String, Command[A]]
  ) extends Parser[A]:
    val paramDefs: Vector[GetOpt.Param] = Vector.empty
    val paramInfos: Vector[ParamInfo] = Vector.empty

    def extract(ctx: ParseContext, reporter: Reporter, result: GetOpt.Result): Result[A] =
      result.subcommand match
        case None =>
          reporter.error("missing command")
          Result.ArgumentError
        case Some(cmd) if subcommands.contains(cmd.name) =>
          subcommands(cmd.name).extract(ctx, Reporter(reporter.stdout, reporter.stderr, cmd.name), cmd.args)
        case Some(cmd) =>
          reporter.error(s"unknown command '${cmd.name}'")
          Result.ArgumentError

  object Subcommand:
    inline def derived[A]: Subcommand[A] = derivedImpl[A].asInstanceOf[Subcommand[A]]
    private inline def derivedImpl[A] = ${
      clam.macros.deriveSubcommand[api.type, A]('api)
    }

  case class Command[+A](
    fieldParsers: IndexedSeq[Parser[?]],
    instantiate: IndexedSeq[_] => A,
    doc: String
  ):
    require(
      fieldParsers.isEmpty || fieldParsers.init.forall(_.subcommands.isEmpty),
      "impossible grammar: only the last scala parameter can be a subcommand"
    )

    val paramDefs = Seq(
      GetOpt.Param("--help", flag = true)
    ) ++ fieldParsers.flatMap(_.paramDefs)

    val subcommands = fieldParsers.flatMap(_.subcommands)

    def extract(ctx: ParseContext, reporter: Reporter, result: GetOpt.Result): Result[A] =
      if !result.knownArgs("--help").isEmpty then
        println(defaultHelpMessage(
          Terminal.current(),
          doc,
          fieldParsers.flatMap(_.paramInfos),
          fieldParsers.flatMap(_.subcommands).toMap.map((k, v) => k -> v.doc)
        ))
        return Result.EarlyExit

      if !result.unknownArgs.isEmpty then
        for arg <- result.unknownArgs do
          reporter.error(s"unknown argument: $arg")
        return Result.ArgumentError

      val fields = new Array[Any](fieldParsers.size)
      var hasErrors: Boolean = false
      var i = 0
      while i < fields.size do
        fieldParsers(i).extract(ctx, reporter, result) match
          case Result.Success(value) => fields(i) = value
          case Result.ArgumentError => hasErrors = true
          case Result.EarlyExit => return Result.EarlyExit
        i += 1

      if hasErrors then return Result.ArgumentError

      try
        Result.Success(instantiate(collection.immutable.ArraySeq.unsafeWrapArray(fields)))
      catch
        case ex: IllegalArgumentException =>
          reporter.error(ex.getMessage)
          Result.ArgumentError

    private def mkCommand(): GetOpt.Command =
      val cmd = GetOpt.Command()
      for param <- paramDefs do cmd.params += param
      for (name, scmd) <- subcommands do cmd.subcommands += name -> scmd.mkCommand()
      cmd

    val command = mkCommand()

    def parse(
      args: Iterable[String],
      ctx: ParseContext = ParseContext(Terminal.current()),
      reporter: Reporter = Reporter(System.out, System.err, "")
    ): Result[A] =
      val opts = GetOpt.parse(mkCommand(), args)
      extract(ctx, reporter, opts)

    def parseOrExit(
      args: Iterable[String],
      ctx: ParseContext = ParseContext(Terminal.current()),
      reporter: Reporter = Reporter(System.out, System.err, ""),
      exit: Int => Nothing = sys.exit
    ): A =
      parse(args, ctx, reporter) match
        case Result.Success(a) => a
        case Result.ArgumentError => exit(2)
        case Result.EarlyExit => exit(0)

  end Command

  object Command:
    def apply[A](using cmd: DerivationApi#Command[A]) = cmd
    inline def derived[A]: Command[A] = derivedImpl[A].asInstanceOf[Command[A]]
    private inline def derivedImpl[A] = ${
      clam.macros.deriveCommand[api.type, A]('api)
    }

  def scalaToNamedParam(scalaName: String): String = s"--${text.kebabify(scalaName)}"
  def scalaToPositionalParam(scalaName: String): String = text.kebabify(scalaName)
  def scalaToSubcommand(scalaName: String): String = text.kebabify(scalaName).toLowerCase()


  // def helpCommand: Command[Nothing] =
  //   new Command(Vector(), _ => ???, ""):
  //     override def extract(ctx: ParseContext, reporter: Reporter, result: GetOpt.Result): Result[Nothing] =
  //       reporter.stdout.println("help")
  //       Result.EarlyExit

  // object Params:
  //   inline def derived[A]: Params[A] = derivedImpl[A].asInstanceOf[Params[A]]
  //   private inline def derivedImpl[A] = ${
  //     DerivationMacros.deriveParams[api.type, A]('api)
  //   }

  /** Signature of a Scala parameter. These are used by the derivation macros to
    * to instantiate parsers for every parameter. Note that this API should be
    * considered experimental. In the future, this typeclass may be eliminated
    * and the behavior folded directly into macros.
    */
  trait Sig[A]:
    def makeParser(scalaName: String, default: Option[() => A], annot: param, doc: String): Parser[A]

  object Sig:
    given simple[A](using reader: Reader[A]): Sig[A] = (scalaName, default, annot, doc) =>
      SingleParam[A](
        name = if annot.name != null then annot.name else if default.isDefined then scalaToNamedParam(scalaName) else scalaToPositionalParam(scalaName),
        default = default,
        flag = false,
        aliases = annot.aliases,
        endOfNamed = annot.endOfNamed,
        reader = reader,
        doc = doc,
        interactiveCompleter = if annot.interactiveCompleter != null then annot.interactiveCompleter else _ => Seq(),
        standaloneCompleter = if annot.standaloneCompleter != null then annot.standaloneCompleter else BashCompleter.Empty
      )
    given flag(using reader: Reader[Boolean]): Sig[Boolean] = (scalaName, default, annot, doc) =>
      SingleParam[Boolean](
        name = if annot.name != null then annot.name else if default.isDefined then scalaToNamedParam(scalaName) else scalaToPositionalParam(scalaName),
        default = default,
        flag = true,
        aliases = annot.aliases,
        endOfNamed = annot.endOfNamed,
        reader = reader,
        doc = doc,
        interactiveCompleter = if annot.interactiveCompleter != null then annot.interactiveCompleter else _ => Seq(),
        standaloneCompleter = if annot.standaloneCompleter != null then annot.standaloneCompleter else BashCompleter.Empty
      )
    given repeated[A, Col[_] <: Iterable[_]](using reader: Reader[A], factory: collection.Factory[A, Col[A]]): Sig[Col[A]] = (scalaName, default, annot, doc) =>
      RepeatedParam[A, Col](
        name = if annot.name != null then annot.name else if default.isDefined then scalaToNamedParam(scalaName) else scalaToPositionalParam(scalaName),
        flag = false,
        aliases = annot.aliases,
        endOfNamed = annot.endOfNamed,
        reader = reader,
        factory = factory,
        doc = doc,
        interactiveCompleter = if annot.interactiveCompleter != null then annot.interactiveCompleter else _ => Seq(),
        standaloneCompleter = if annot.standaloneCompleter != null then annot.standaloneCompleter else BashCompleter.Empty
      )
    given repeatedFlag[Col[_] <: Iterable[_]](using reader: Reader[Boolean], factory: collection.Factory[Boolean, Col[Boolean]]): Sig[Col[Boolean]] = (scalaName, default, annot, doc) =>
      RepeatedParam[Boolean, Col](
        name = if annot.name != null then annot.name else if default.isDefined then scalaToNamedParam(scalaName) else scalaToPositionalParam(scalaName),
        flag = true,
        aliases = annot.aliases,
        endOfNamed = annot.endOfNamed,
        reader = reader,
        factory = factory,
        doc = doc,
        interactiveCompleter = if annot.interactiveCompleter != null then annot.interactiveCompleter else _ => Seq(),
        standaloneCompleter = if annot.standaloneCompleter != null then annot.standaloneCompleter else BashCompleter.Empty
      )
    given explicit[A](using p: Parser[A]): Sig[A] = (_, _, _, _) => p
  end Sig
