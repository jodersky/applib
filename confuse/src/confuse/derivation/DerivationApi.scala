package confuse.derivation

import confuse.model.Value
import confuse.model.Arr
import confuse.model.Config
import confuse.model.Str
import confuse.model.Null

enum ReadResult[+A]:
  case Success(a: A)
  case Error(m: String)

trait ReaderApi:

  trait Reader[A]:
    def read(cfg: Value): A
  object Reader


trait StandardReaders extends ReaderApi with clam.core.StandardStringReaders:

  given primitiveReader[A](using sr: StringReader[A]): Reader[A] with
    def read(cfg: Value): A = cfg match
      case Str(s) =>
        sr.read(s) match
          case StringReader.Result.Success(a) => ReadResult.Success(a)
          case StringReader.Result.Error(m) => ReadResult.Error(m)

        ???

trait DerivationApi extends ReaderApi:

  extension (r: Reader.type)
    def derive[A]: Reader[A] = ???

//   trait Reader[A]:
//     def read(cfg: Config): A

//   object Reader
