package clam.dispatch

import clam.completion
import clam.getopt

/** A CLI parameter definition.
  *
  * Includes properties for parsing as well as for auxiliary utilities such as
  * help messages and completion.
  */
case class ParamDef(
  names: Seq[String],
  description: String,
  argName: Option[String], // if this is a named param, what should the argument be called in help messages
  repeats: Boolean = false,
  endOfNamed: Boolean = false,
  interactiveCompleter: String => Iterable[String] = _ => Seq(),
  standaloneCompleter: completion.BashCompleter = completion.BashCompleter.Default
):
  require(!names.isEmpty, "a parameter must have at least one name")

  def isNamed = names.head.startsWith("-")
  def isFlag = isNamed && argName.isEmpty

  /** CLI grammar */
  def parseInfo = getopt.Param(
    name = names.head,
    aliases = names.tail,
    flag = isFlag,
    positionalRepeats = repeats && !isNamed,
    endOfNamed = endOfNamed
  )

  /** Information for generating completion */
  def completionInfo: completion.Param = completion.Param(
    names, repeats, isFlag, standaloneCompleter
  )
