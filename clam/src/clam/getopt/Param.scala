package clam.getopt

import collection.mutable

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
