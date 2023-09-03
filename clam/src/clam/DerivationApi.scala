package clam

case class ParseContext(
  terminal: Terminal
)

object Command:
  def apply[A](using cmd: DerivationApi#Command[A]) = cmd

trait DerivationApi extends ReaderApi:
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
    def subcommands: Map[String, Command[?]] = Map.empty
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
    def paramDefs = Vector(pdef)
    def paramInfos = Vector(pinfo)

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
    def paramDefs: Vector[GetOpt.Param] = Vector.empty
    def paramInfos: Vector[ParamInfo] = Vector.empty

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
      DerivationMacros.deriveSubcommand[api.type, A]('api)
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

    def paramDefs = Seq(
      GetOpt.Param("--help", flag = true)
    ) ++ fieldParsers.flatMap(_.paramDefs)

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
      for parser <- fieldParsers do
        for (name, scmd) <- parser.subcommands do
          cmd.subcommands += name -> scmd.mkCommand()
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
    inline def derived[A]: Command[A] = derivedImpl[A].asInstanceOf[Command[A]]
    private inline def derivedImpl[A] = ${
      DerivationMacros.deriveCommand[api.type, A]('api)
    }

  def scalaToNamedParam(scalaName: String): String = s"--${text.kebabify(scalaName)}"
  def scalaToPositionalParam(scalaName: String): String = text.kebabify(scalaName)
  def scalaToSubcommand(scalaName: String): String = text.kebabify(scalaName.toLowerCase())


  /** Generate a help message from parameters.
    *
    * This message will be used by `ArgumentParser`s. Overriding this allows you
    * to customize the help message of all `ArgumentParser`s.
    */
  def defaultHelpMessage(
    term: Terminal,
    description: String,
    paramInfos: Iterable[ParamInfo],
    subcommands: Iterable[(String, String)]
  ): String = {
    val (named0, positional) = paramInfos.toSeq.partition(_.isNamed)
    val named = named0.sortBy(_.names.head)

    val b = new StringBuilder
    b ++= s"Usage:"
    if (!named.isEmpty) {
      b ++= " [OPTIONS]"
    }
    for (param <- positional) {
      b ++= " "
      b ++= param.names.head.toUpperCase
      if (param.repeats) b ++= "..."
    }
    b ++= "\n"

    if (!description.isEmpty()) {
      b ++= "\n"
      b ++= description
      b ++= "\n\n"
    }

    // Note that not necessarily all named parameters must be optional. However
    // since that is usually the case, this is what the default help message
    // assumes.
    if (!named.isEmpty) {
      b ++= "Options:\n"

      // -short, --long tpe wrapped
      val lhs = for (param <- named) yield {
        val long = param.names.head
        val short = if (long.length == 2) "" else param.names.find(_.length == 2).getOrElse("")
        val argname = param.argName.getOrElse("")

        if (short != "") {
          s"  $short, $long $argname  "
        } else {
          s"      $long $argname  "
        }
      }

      val col1Width = lhs.map(_.length).max
      val col2Width = term.cols.getOrElse(80) - col1Width

      if (col2Width > 30) {
        for ((l, param) <- lhs.zip(named)) {
          b ++= l
          b ++= " " * (col1Width - l.length)
          text.wrap(param.description, b, col2Width, "\n" + " " * col1Width)
          b += '\n'
        }
      } else {
        for ((l, param) <- lhs.zip(named)) {
          b ++= l
          b += '\n'
          b ++= param.description
          b += '\n'
        }
      }
    }

    if (!subcommands.isEmpty) {
      val width = subcommands.map(_._1.length).max + 3

      b ++= "Commands:\n"
      for ((name, cmdDesc) <- subcommands) {
        b ++= "  "
        b ++= name
        b ++= " "
        b ++= " " * (width - name.length)
        text.wrap(cmdDesc, b, term.cols.getOrElse(80) - width, "\n" + " " * width)
        b ++= "\n"
      }
    }

    // val describedPos = positional.filter(!_.description.isEmpty)
    // if (!describedPos.isEmpty) {
    //   b ++= "positional arguments:\n"
    //   for (param <- positional) {
    //     b ++= s"  ${param.names.head}\n        "
    //     TextUtils.wrap(param.description, b, width, "\n        ")
    //     b ++= "\n"
    //   }
    // }

    b.result()
  }

  // object Params:
  //   inline def derived[A]: Params[A] = derivedImpl[A].asInstanceOf[Params[A]]
  //   private inline def derivedImpl[A] = ${
  //     DerivationMacros.deriveParams[api.type, A]('api)
  //   }

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

object DerivationMacros:
  import quoted.Expr
  import quoted.Quotes
  import quoted.Type

  private def getDefaultParams(using qctx: Quotes)
    (method: qctx.reflect.Symbol): Map[qctx.reflect.Symbol, Expr[_]] =
    import qctx.reflect.*
    val pairs = for
      (param, idx) <- method.paramSymss.flatten.zipWithIndex
      if (param.flags.is(Flags.HasDefault))
    yield
      val term = if method.isClassConstructor then
        val defaultName = s"$$lessinit$$greater$$default$$${idx + 1}"
        Ref(method.owner.companionModule.methodMember(defaultName).head)
      else
        val defaultName = s"${method.name}$$default$$${idx + 1}"
        Ref(method.owner.methodMember(defaultName).head)
      param -> term.asExpr
    pairs.toMap

  def call(using qctx: Quotes)(
    method: qctx.reflect.Symbol,
    argss: Expr[Seq[Seq[?]]]
  ): Expr[?] =
    import quoted.quotes.reflect.*

    val base: Term = if method.isClassConstructor then
      Select(New(TypeTree.ref(method.owner)), method)
    else
      Ref(method)

    val paramss = method.paramSymss

    val accesses =
      for i <- paramss.indices.toList yield
        for j <- paramss(i).indices.toList yield
          paramss(i)(j).termRef.widenTermRefByName.asType match
            case '[t] =>
              '{$argss(${Expr(i)})(${Expr(j)}).asInstanceOf[t]}.asTerm

    val application = accesses.foldLeft(base)((lhs, args) => Apply(lhs, args))
    application.asExpr
  end call

  def deriveCommand[Api <: DerivationApi, A: Type](using qctx: Quotes)(api: Expr[DerivationApi]): Expr[DerivationApi#Command[?]] =
    import qctx.reflect.*

    val tpe = TypeRepr.of[A]
    if !(tpe <:< TypeRepr.of[Product]) then
      report.error(s"${tpe.show} is not a product type")
      return '{???}

    deriveCommandSymbol[Api](api, tpe.typeSymbol)
  end deriveCommand

  def deriveCommandSymbol[Api <: DerivationApi](using qctx: Quotes)(
    api: Expr[DerivationApi],
    tsym: qctx.reflect.Symbol
  ): Expr[DerivationApi#Command[?]] =
    import qctx.reflect.*

    val constructor = tsym.primaryConstructor //tsym.companionModule.methodMember("apply").head

    val doc = DocComment.extract(tsym.docstring.getOrElse(""))
    val parsers = Expr.ofList(paramParsers(api.asTerm, constructor, doc))
    '{
      val p = $api
      new p.Command(
        $parsers.toVector.asInstanceOf[Vector[p.Parser[_]]],
        fields =>
          val s = Seq(fields.toSeq)
          ${call(using qctx)(constructor, 's)},
        ${Expr(doc.paragraphs.mkString("\n"))}
      )
    }

  def deriveSubcommand[Api <: DerivationApi, A: Type](using qctx: Quotes)(api: Expr[DerivationApi]): Expr[DerivationApi#Subcommand[A]] =
    import qctx.reflect.*
    val tpe = TypeRepr.of[A]

    tpe.typeSymbol.children match
      case Nil =>
        val flags = tpe.typeSymbol.flags
        if flags.is(Flags.Enum) || flags.is(Flags.Sealed) then
          report.error("A subcommand must have at least one child.")
        else
          report.error("A subcommand can only be derived for an enum or a sealed trait.")
        '{???}
      case children =>
        val subcommandNames = Expr.ofList(children.map(child => Expr(child.name)))
        val subcommands = Expr.ofList(
          children.map(child => deriveCommandSymbol(api, child))
        )

        '{
          val p = $api
          val names = $subcommandNames.map(n => p.scalaToSubcommand(n))
          val values = $subcommands.asInstanceOf[List[p.Command[A]]]
          new p.Subcommand[A](
            names.zip(values).toMap
          )
        }

  /** Generate a parser for each parameter of a method.
    * @param api A term of an Expr[_ <: DerivationApi]. The type prefix is used
    * to look up implicits and allow a user to configure custom API flavors of
    * reusable parsers.
    * @param method the method
  */
  private def paramParsers(using qctx: Quotes)(
    api: qctx.reflect.Term,
    method: qctx.reflect.Symbol,
    doc: DocComment
  ): List[Expr[DerivationApi#Parser[Any]]] =
    import qctx.reflect.*

    val defaults = getDefaultParams(method)

    def summonParserBuilder(tpe: TypeRepr): Option[Term] =
      val readerType =
        TypeSelect(
          api,
          "Sig"
        ).tpe.appliedTo(List(tpe))
      Implicits.search(readerType) match
        case iss: ImplicitSearchSuccess => Some(iss.tree)
        case other => None

    for param  <- method.paramSymss.flatten yield
      val paramTpe = param.termRef.widenTermRefByName

      val annot = param.getAnnotation(TypeRepr.of[clam.param].typeSymbol) match
        case None => '{clam.param()}
        case Some(a) => a.asExprOf[clam.param]

      summonParserBuilder(paramTpe) match
        case None =>
          report.error(s"No parser available for parameter ${param.name}.", param.pos.get)
          '{???}

        case Some(term) =>
          paramTpe.asType match
            case '[t] =>
              '{
                val p = ${api.asExpr}.asInstanceOf[DerivationApi]
                val builder = ${term.asExpr}.asInstanceOf[p.Sig[t]]
                val default = ${
                  defaults.get(param) match
                    case None => '{None}
                    case Some(d) => '{Some(() => ${d.asExprOf[t]})}
                }
                val paramdoc = ${Expr(doc.params.getOrElse(param.name, ""))}

                val parser = builder.makeParser(${Expr(param.name)}, default, $annot, paramdoc)
                parser
              }
