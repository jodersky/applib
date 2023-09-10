package clam.api

trait StandardReaders extends ReaderApi with clam.core.StandardStringReaders:
  import Reader.Result

  // given a common string reader, convert it to a CLI string reader
  given [A](using r: StringReader[A]): Reader[A] with
    def read(arg: String): Result[A] = r.read(arg) match
      case StringReader.Result.Success(a) => Result.Success(a)
      case StringReader.Result.Error(a) => Result.Error(a)
