package clam.derivation

import clam.completion

/** Used to customize or override parameter derivation.
  *
  * @param name Set the name explicitly rather than derive it.
  */
class param(
  val name: String | Null = null, // we use String | Null instead of Option[String] since this will be set only by user code
  val aliases: Seq[String] = Seq(),
  val endOfNamed: Boolean = false,
  val interactiveCompleter: (String => Seq[String]) | Null = null,
  val standaloneCompleter: completion.BashCompleter | Null = null,
  val argName: String | Null = null,
  val reader: clam.readers.ReaderApi#Reader[?] | Null = null
) extends annotation.StaticAnnotation
