package clam.derivation

trait Api extends DerivationApi with clam.readers.StandardReaders with StandardCompleters

object DefaultApi extends Api
