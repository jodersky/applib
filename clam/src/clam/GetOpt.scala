package clam

import collection.mutable

/** A utility to sort and extract arguments from a set of parameter definitions. */
object GetOpt:

  /** A parameter definition for a [[GetOpt]] instance.
    *
    * The name determines the type of parameter: if it starts with "-", then the
    * parameter will be considered *named*, otherwise it will be considered
    * *positional*.
    *
    * A named parameter can occur in any position on the command line and its
    * arguments will be taken as the word after encountering the name.
    * Positional parameters always take an argument based on the position in
    * which they are declared.
    *
    * E.g. `--named1 arg1 pos1 -named2 arg2 --named3=arg3 pos`, would assign the
    * arguments to parameters in the following way:
    * - named arguments:
    *   - `--named1` is assigned `arg1`
    *   - `-named2` is assigned `arg2`
    *   - `--named3` is assigned `arg3`
    * - positional arguments:
    *   - first positional parameter is assigned `pos1` (regardless of name of
    *     the parameter)
    *   - second positional parameter is assigned `pos2` (regardless of name of
    *     the parameter)
    *
    * @param name The primary name of the parameter. This name can be used to
    * look up arguments after calling [[GetOpt.parse]].
    *
    * @param aliases Other names which can be used for this parameter. This is
    * only useful for named parameters, and is typically used to declare
    * short-form parameters. E.g. name `--force`, alias `-f`.
    *
    * @param flag A parameter declared as a flag never takes an argument. This
    * is only useful for named parameters.
    *
    * @param positionalRepeats If this parameter is positional, it will be
    * repeated indefinitely.
    *
    * @param endOfNamed After encountering this parameter, all subsequent command
    * line arguments will be interpreted as positional, regardless if they start
    * with a `-` or not.
    */
  case class Param(
      name: String,
      aliases: Seq[String] = Seq(),
      flag: Boolean = false,
      positionalRepeats: Boolean = false,
      endOfNamed: Boolean = false
  ):
    require(
      name != "--",
      "-- is not a valid parameter name; it is used by the parser to explicitly delimit positional parameters"
    )
    if name.startsWith("-") then
      require(
        aliases.forall(_.startsWith("-")),
        "named and positional parameters must not share definitions"
      )
    def isNamed = name.startsWith("-")

  // extractor for named arguments
  private val Named = "(--?[^=]+)(?:=(.*))?".r

  // used for special-casing short parameters
  private val ShortNamed = "-([^-].*)".r

  /** A known argument corresponding to a parameter
    *
    * @param nameUsed In case of a named param, the actual name of the
    * parameter used. This is used to disambiguate between aliases in
    * certain situations, for example in error reporting.
    * @param arg In case of a named param, the argument given after
    * the name, if any. In case of a positional param, this value is
    * always set.
    */
  case class Arg(
    nameUsed: String,
    arg: Option[String]
  )

  case class Result(
    knownArgs: mutable.LinkedHashMap[String, mutable.ArrayBuffer[Arg]],
    unknownArgs: mutable.ArrayBuffer[String],
    subcommand: Option[SubcommandResult] = None // this is only populated if a subcommand has been set in the def
  )

  case class SubcommandResult(
    name: String,
    args: Result
  )

  case class Command(
    params: mutable.ArrayBuffer[Param] = mutable.ArrayBuffer.empty,
    subcommands: mutable.LinkedHashMap[String, Command] = mutable.LinkedHashMap.empty
  )

  def parse(
    params: Iterable[Param],
    args: Iterable[String]
  ): Result =
    val cmd = Command()
    cmd.params ++= params
    parse(cmd, args)

  def parse(
    command: Command,
    args: Iterable[String]
  ): Result = parse(command, args.iterator)

  def parse(
    command: Command,
    args: Iterator[String]
  ): Result =
    val params = command.params
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
    val unknownArgs = mutable.ArrayBuffer.empty[String]
    val parsedArgs = mutable.LinkedHashMap.empty[String, mutable.ArrayBuffer[Arg]]

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

    var subcommand: String = null
    var onlyPositionals = false
    def addPositional() =
      if pos < positional.length then
        val param = positional(pos)
        val argList = parsedArgs(param.name)
        argList += Arg(param.name, Some(arg))
        if param.endOfNamed then onlyPositionals = true
        if !param.positionalRepeats then pos += 1
        readArg()
      else if !command.subcommands.isEmpty then
        subcommand = arg
        arg = null // stop parsing
      else
        unknownArgs += (arg)
        readArg()

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
            readArg()
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
                unknownArgs += name
              }
            }
          case Named(name, _) =>
            unknownArgs += name
            readArg()
          case positional =>
            addPositional()
        }
      }
    end while

    if subcommand == null then
      Result(
        parsedArgs,
        unknownArgs,
        None
      )
    else
      command.subcommands.get(subcommand) match
        case None =>
          val childResult = SubcommandResult(
            subcommand,
            Result(
              knownArgs = mutable.LinkedHashMap.empty, // no known args for unknown subcommand
              unknownArgs = mutable.ArrayBuffer.from(args), // unknown subcommand gets all remaining args as unknowns
              subcommand = None
            )
          )
          Result(
            parsedArgs,
            unknownArgs,
            Some(childResult)
          )
        case Some(scmd) =>
          Result(
            parsedArgs,
            unknownArgs,
            Some(SubcommandResult(subcommand, parse(scmd, args)))
          )
  end parse
