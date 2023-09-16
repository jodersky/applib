package clam.derivation

import clam.completion
import clam.getopt

/** A CLI parameter definition.
  *
  * Includes properties for parsing as well as for auxiliary utilities such as
  * help messages and completion.
  */
case class ParamDef(
  names: Seq[String],
  argName: Option[String], // if this is a named param, what should the argument be called in help messages
  repeats: Boolean,
  description: String,
  endOfNamed: Boolean,
  interactiveCompleter: String => Iterable[String],
  standaloneCompleter: completion.BashCompleter
):
  require(!names.isEmpty, "a parameter must have at least one name")

  def isNamed = names.head.startsWith("-")
  def isFlag = isNamed && argName.isEmpty

  def parseInfo = getopt.Param(
    name = names.head,
    aliases = names.tail,
    flag = isFlag,
    positionalRepeats = repeats && !isNamed,
    endOfNamed = endOfNamed
  )

  def completionInfo: completion.Param = completion.Param(
    names, repeats, isFlag, standaloneCompleter
  )
