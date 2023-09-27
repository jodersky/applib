package clam.dispatch

import collection.mutable as m
import clam.util.Terminal
import clam.getopt

case class Context(
  reporter: Reporter,
  terminal: Terminal,
  env: m.Map[String, String] = m.Map()
)


// An entrypoint to the application

// A command can be in a tree, but it is designed to be callable as a top-level
// entrypoint. As such, parent args must be stored in a global context, usually
// the environment.

// class Command[-Parent, +Args](
//   name: String,
//   pdefs: Seq[getopt.Param],
//   extract: (Context, Parent, getopt.Result) => Result[Args]
// ):
//   ()
  // private var _invoke: Option[Args => Result[Any]] = None
  // private invokeSubcommand: Option[(String, Context, Args, Iterator[String]) => Result[Any]]

