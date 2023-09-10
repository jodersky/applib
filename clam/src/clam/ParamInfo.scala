package clam

/** User-friendly parameter information, used for generating help message */
case class ParamInfo(
    isNamed: Boolean,
    names: Seq[String],
    argName: Option[String], // if this is a named param, what should the argument be called in help messages
    repeats: Boolean,
    description: String,
    interactiveCompleter: String => Seq[String],
    standaloneCompleter: BashCompleter
):
  def isFlag = isNamed && argName == None

