package clam.derivation

import clam.util.Terminal

case class ParseCtx(
  reporter: Reporter,
  terminal: Terminal,
  commandChain: collection.mutable.ListBuffer[String] = collection.mutable.ListBuffer.empty
)
