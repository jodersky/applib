package clam.getopt

import collection.mutable

/** A utility to sort and extract arguments from a set of parameter definitions. */
object `package`

// extractor for named arguments
private val Named = "(--?[^=]+)(?:=(.*))?".r

// used for special-casing short parameters
private val ShortNamed = "-([^-].*)".r

def parse(
  params: Iterable[Param],
  args: Iterable[String]
): Result =
  parse(params, args.iterator)

def parse(
  params: Iterable[Param],
  args: Iterator[String]
): Result =
  val aliasMap = mutable.Map.empty[String, Param] // map of all possible names of named params
  val positional = mutable.ArrayBuffer.empty[Param]

  // populate parameter defs
  params.foreach { p =>
    if (p.isNamed) {
      aliasMap += p.name -> p
      p.aliases.foreach { n => aliasMap += n -> p }
    } else {
      positional += p
    }
  }
  val parsedArgs = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[Arg]]
  var unknown: String = null

  for param <- params do
    parsedArgs(param.name) = mutable.ArrayBuffer.empty

  var pos = 0 // index of current positional argument

  var arg: String = null
  def readArg() =
    if args.hasNext then
      arg = args.next()
    else
      arg = null
  readArg()

  var onlyPositionals = false
  def addPositional() =
    if pos < positional.length then
      val param = positional(pos)
      val argList = parsedArgs(param.name)
      argList += Arg(param.name, Some(arg))
      if param.endOfNamed then onlyPositionals = true
      if !param.positionalRepeats then pos += 1
      readArg()
    else
      unknown = arg
      arg = null // stop parsing

  while arg != null do
    if (onlyPositionals) {
      addPositional()
    } else {
      arg match {
        case "--" =>
          onlyPositionals = true
          readArg()
        case Named(name, embedded) if aliasMap.contains(name) =>
          readArg()
          val param = aliasMap(name)
          val argList = parsedArgs(param.name)
          if (embedded != null) { // embedded argument, i.e. one that contains '='
            argList += Arg(name, Some(embedded))
          } else if (param.flag) { // flags never take an arg

            argList += Arg(name, None)
          } else if (arg == null || arg.matches(Named.regex)) { // non-flags may have an arg
            argList += Arg(name, None)
          } else {
            argList += Arg(name, Some(arg))
            readArg()
          }
          if (param.endOfNamed) onlyPositionals = true
        case ShortNamed(name) =>

          // deal with combined single-letter options (the previous case
          // already took care of any named args that are known, long and
          // short)
          val letters = name.iterator
          while (letters.hasNext) {
            val option = s"-${letters.next()}"
            if (aliasMap.contains(option)) {
              val param = aliasMap(option)
              val argList = parsedArgs.getOrElseUpdate(param.name, mutable.ArrayBuffer.empty)
              if (param.flag) { // flags never take an arg
                argList += Arg(option, None)
              } else if (letters.hasNext) {
                argList += Arg(option, Some(letters.mkString))
              } else {
                argList += Arg(option, None)
              }
              if (param.endOfNamed) onlyPositionals = true
            } else {
              // In case of an unknown short letter, the argument is reported
              // unknown as in the regular named case. In other words, the
              // remaining letters are consumed and any embedded values are
              // omitted
              val Named(name, _) = s"$option${letters.mkString}": @unchecked
              unknown = name
              arg = null // stop parsing
            }
          }
          if arg != null then readArg()
        case Named(_, _) =>
          unknown = arg
          arg = null // stop parsing
        case positional =>
          addPositional()
      }
    }
  end while

  if unknown == null then
    Result(
      parsedArgs,
      None
    )
  else
    Result(
      parsedArgs,
      Some((unknown, args))
    )
end parse
