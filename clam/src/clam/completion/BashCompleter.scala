package clam.completion

enum BashCompleter:

  /** No completion */
  case Empty

  /** Completion picked from a fixed set of words */
  case Fixed(alternatives: Iterable[String])

  /** Default bash completion (uses paths) */
  case Default

  /** A plain bash snippet that is executed during completion.
    *
    * This should typically either set `COMPREPLY` or call `compopt`.
    */
  case Raw(script: String)
