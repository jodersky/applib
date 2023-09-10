package clam

/** Used to customize or override parameter derivation.
  *
  * @param name Set the name explicitly rather than derive it.
  */
class param(
  val name: String = null,
  val aliases: Seq[String] = Seq(),
  val endOfNamed: Boolean = false,
  val interactiveCompleter: String => Seq[String] = null,
  val standaloneCompleter: BashCompleter = null
) extends annotation.StaticAnnotation
