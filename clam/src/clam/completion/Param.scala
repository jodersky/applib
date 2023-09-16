package clam.completion

case class Param(
  names: Seq[String],
  repeats: Boolean,
  isFlag: Boolean,
  standaloneCompleter: BashCompleter
):
  def isNamed = names.head.startsWith("-")
