package clam.dispatch

import collection.mutable as m
import clam.util.Terminal
import clam.getopt

case class Context(
  stdout: java.io.PrintStream,
  stderr: java.io.PrintStream,
  env: m.Map[String, String],
  terminal: Terminal
)
