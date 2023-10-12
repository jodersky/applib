package clam.derivation2

import clam.completion

/** Annotate to *scala* parameters to customize or override cli parameter
  * derivation.
  *
  * Since this annotation will be set by user code, in order to reduce
  * boilerplate, optional parameters of this class are declared as `A | Null`
  * instead of `Option[A]`, with a default `null` value. Thus, a user who wishes
  * to set a parameter can simply assign it without having to wrap it in a
  * `Some`. E.g. `@param(name = "foo")`. Any unset parameter means "use the
  * default behavior from derivation".
  *
  * @param name Set the name explicitly rather than derive it.
  * @param aliases Additional names by which this parameter can be set.
  * @param endOfNamed If set, then all subsequent parameters will be treated as
  * positional, regardless of their name.
  * @param interactiveCompleter A function invoked during command completion.
  * Given a partially types argument, returns all possible value for said
  * argument.
  * @param standaloneCompleter A builtin completer when completion is handles by
  * external scripts.
  * @param argName The name of the argument given to a named arg. E.g. `<path>`
  * in `--base-dir=<path>`.
  * @param reader Override the reader used to used to deserialize a string to a
  * scala value. Note that the type of the reader must match the type of scala
  * parameter definition that this annotation is applied to.
  */
class param(
  val name: String | Null = null,
  val aliases: Seq[String] = Seq(),
  val endOfNamed: Boolean = false,
  val interactiveCompleter: (String => Seq[String]) | Null = null,
  val standaloneCompleter: completion.BashCompleter | Null = null,
  val argName: String | Null = null,
  val reader: clam.readers.ReaderApi#Reader[?] | Null = null
) extends annotation.StaticAnnotation
