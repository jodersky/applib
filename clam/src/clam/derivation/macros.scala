package clam.derivation.macros

import clam.derivation.DerivationApi
import clam.derivation.Def
import clam.dispatch.Command

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

private def call(using qctx: Quotes)(
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

private case class DocComment(paragraphs: Iterable[String], params: collection.Map[String, String])

private object DocComment:

  private val Param = """@param\s+(\w+)\s*(.*)""".r

  def extract(comment: String): DocComment =
    val content = comment.drop(3).dropRight(2) // all doc comments start with /** and end with */
    val lines = content.linesIterator

    val paragraphs = collection.mutable.ListBuffer.empty[String]
    val paragraph = StringBuilder()

    var line: String = ""
    var eof = false

    def readLine() =
      eof = !lines.hasNext
      if !eof then
        line = lines.next().dropWhile(_ == ' ').dropWhile(_ == '*').trim

    def readParagraph() =
      paragraph.clear()
      if !eof && !line.isEmpty() then
        paragraph ++= line
        readLine()
        while !eof && !line.isEmpty() && !line.startsWith("@") do
          paragraph += ' '
          paragraph ++= line
          readLine()
        end while

    def readParagraphs() =
      paragraphs.clear()
      while !eof && !line.startsWith("@") do
        while !eof && line.isEmpty() do readLine() // skip blanks
        if !eof && !line.startsWith("@") then
          readParagraph()
          paragraphs += paragraph.result()

    // readLine()
    var mainDoc: List[String] = Nil
    if !eof then
      readParagraphs()
      mainDoc = paragraphs.result()

    val params = collection.mutable.LinkedHashMap.empty[String, String]
    while !eof do
      line match
        case Param(name, rest) =>
          line = rest
          readParagraphs()
          params += name -> paragraphs.result().mkString(" ")
        case _ => readParagraph()
    end while

    DocComment(mainDoc, params)
  end extract

def commandFor[Container: Type](container: Expr[Container])(using qctx: Quotes): Expr[Command] =
  import qctx.reflect.*

  collectApiWithAnnot[Container, DerivationApi#command] match
    case (method, api) :: Nil =>
      commandForMethod(api, method)
    case _ =>
      report.error("exactly one method must be annotated with @command")
      '{???}

def commandsFor[Container: Type](container: Expr[Container])(using qctx: Quotes): Expr[List[Command]] =
  import qctx.reflect.*

  val cmds = for (method, api) <- collectApiWithAnnot[Container, DerivationApi#command] yield
    commandForMethod(api, method)

  Expr.ofList(cmds)

// find methods annotated with a given path-dependent annotation and return the
// parent term
private def collectApiWithAnnot[Container: Type, Annot: Type](using qctx: Quotes): List[(qctx.reflect.Symbol, Expr[DerivationApi])] =
  import qctx.reflect.*

  for
    method <- TypeRepr.of[Container].typeSymbol.methodMembers
    annot = method.annotations.find(_.tpe <:< TypeRepr.of[Annot])
    if annot.isDefined
  yield
    val TypeRef(apiTerm: TermRef, _) = annot.get.tpe: @unchecked
    val apiExpr = Ref.term(apiTerm).asExprOf[DerivationApi]
    method -> apiExpr

private def commandForMethod(using qctx: Quotes)(
  api: Expr[DerivationApi],
  method: qctx.reflect.Symbol
): Expr[Command] =
  import qctx.reflect.*

  val doc = DocComment.extract(method.docstring.getOrElse(""))
  val defs = Expr.ofList(paramDefs(api.asTerm, method, doc))
  '{
    val p = $api
    val cmd = Command(
      name = ${Expr(method.name)},
      description = ${Expr(doc.paragraphs.mkString("\n"))},
    )
    cmd.pdefs ++= p.extraParams
    cmd.pdefs ++= $defs.flatMap(_.pdefs)
    cmd.parse = p.parse($defs)
    cmd.action = Some(
      (args: Any) =>
        val s = Seq(args.asInstanceOf[Seq[?]])
        val r = ${call(using qctx)(method, 's)}
        clam.dispatch.Result.Success(r)
    )
    cmd
  }

/** Generate a parser for each parameter of a method.
  * @param api A term of an Expr[_ <: DerivationApi]. The type prefix is used
  * to look up implicits and allow a user to configure custom API flavors of
  * reusable parsers.
  * @param method the method
*/
private def paramDefs(using qctx: Quotes)(
  api: qctx.reflect.Term,
  method: qctx.reflect.Symbol,
  doc: DocComment
): List[Expr[Def[Any]]] =
  import qctx.reflect.*

  val defaults = getDefaultParams(method)

  def summonParserBuilder(tpe: TypeRepr): Option[Term] =
    val readerType =
      TypeSelect(
        api,
        "Parser"
      ).tpe.appliedTo(List(tpe))
    Implicits.search(readerType) match
      case iss: ImplicitSearchSuccess => Some(iss.tree)
      case other => None

  for param  <- method.paramSymss.flatten yield
    val paramTpe = param.termRef.widenTermRefByName

    val annot = param.getAnnotation(TypeRepr.of[clam.derivation.param].typeSymbol) match
      case None => '{clam.derivation.param()}
      case Some(a) => a.asExprOf[clam.derivation.param]

    summonParserBuilder(paramTpe) match
      case None =>
        report.error(s"No parser ${param.termRef.widenTermRefByName} available for parameter ${param.name}.", param.pos.get)
        '{???}

      case Some(term) =>
        paramTpe.asType match
          case '[t] =>
            '{
              val p = ${api.asExpr}.asInstanceOf[DerivationApi]
              val parser = ${term.asExpr}.asInstanceOf[p.Parser[t]]
              val default = ${
                defaults.get(param) match
                  case None => '{None}
                  case Some(d) => '{Some(() => ${d.asExprOf[t]})}
              }
              val paramdoc = ${Expr(doc.params.getOrElse(param.name, ""))}

              parser.make(Seq(${Expr(param.name)}), default, $annot, paramdoc)
            }
