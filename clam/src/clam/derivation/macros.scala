package clam.derivation.macros

import clam.derivation.DerivationApi
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


def deriveCommand[Api <: DerivationApi, A: Type](using qctx: Quotes)(api: Expr[DerivationApi]): Expr[DerivationApi#Command[?]] =
  import qctx.reflect.*

  val tpe = TypeRepr.of[A]
  if !(tpe <:< TypeRepr.of[Product]) then
    report.error(s"${tpe.show} is not a product type")
    return '{???}

  deriveCommandSymbol[Api](api, tpe.typeSymbol)
end deriveCommand

private def deriveCommandSymbol[Api <: DerivationApi](using qctx: Quotes)(
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
      ${Expr(clam.text.kebabify(tsym.name).toLowerCase())},
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
        "ScalaParam"
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
              val builder = ${term.asExpr}.asInstanceOf[p.ScalaParam[t]]
              val default = ${
                defaults.get(param) match
                  case None => '{None}
                  case Some(d) => '{Some(() => ${d.asExprOf[t]})}
              }
              val paramdoc = ${Expr(doc.params.getOrElse(param.name, ""))}

              val parser = builder.makeParser(${Expr(param.name)}, default, $annot, paramdoc)
              parser
            }