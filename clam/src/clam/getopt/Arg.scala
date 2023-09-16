package clam.getopt

import collection.mutable

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
  unknown: Option[(String, Iterator[String])]
)
