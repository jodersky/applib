package clam

import clam.util.Terminal

// Note: ideally we'd use only plain exports instead of a package object and
// inheritance. Because of a compiler bug however, exported mehods can lose
// their default parameters. Hence, until https://github.com/lampepfl/dotty/issues/17930
// we'll need to revert to using a package object.
object `package`:

  inline def commandFor[A <: AnyRef](container: A) = ${
    derivation.macros.commandFor('container)
  }
  inline def commandsFor[A <: AnyRef](container: A) = ${
    derivation.macros.commandsFor('container)
  }

  export clam.derivation.param
  export clam.dispatch.Command
  export clam.dispatch.Result

  object default
    extends derivation.DerivationApi
    with readers.StandardReaders
    with derivation.StandardCompleters
