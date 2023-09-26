package confuse.model


import collection.mutable
import confuse.util.SourcePos

/** Marker trait for terminal values.
  *
  * A terminal value does not contain other values.
  */
sealed trait Terminal extends Value

sealed trait Value:

  /** All origins that were used to get this value. */
  var origins: List[Origin] = Nil

  def flatten(path: Path = Nil): mutable.LinkedHashMap[String, Terminal] =
    val buffer = mutable.LinkedHashMap.empty[String, Terminal]
    flattenInto(path, buffer)
    buffer

  def flattenInto(path: Path = Nil, buffer: mutable.LinkedHashMap[String, Terminal]): Unit =
    this match
      case t: Terminal =>
        buffer += path.segments.mkString(".") -> t
      case arr: Arr =>
        for (elem, idx) <- arr.elems.zipWithIndex do
          elem.flattenInto(Path(path.segments :+ idx.toString), buffer)
      case cfg: Config =>
        for (key, value) <- cfg.fields do
          value.flattenInto(Path(path.segments :+ key), buffer)

  def dumpInto(path: Path = Nil, out: java.io.PrintStream = System.err): Unit =
    for (key, terminal) <- flatten(path).toSeq.sortBy(_._1) do
      out.print(key)
      terminal match
        case s: Str => out.print(s"='${s.str}'")
        case _: Null => out.print(s"=null")
      out.print(" from ")

      terminal.origins match
        case Nil => out.println("<unknown>")
        case head :: _ => out.println(head.pretty)

  def dump(path: Path = Nil): String =
    val baos = java.io.ByteArrayOutputStream()
    dumpInto(path, java.io.PrintStream(baos))
    new String(baos.toByteArray(), "utf-8")

  // def getValue(path: Path): Value =
  //   path.segments match
  //     case Nil => this
  //     case head :: tail =>
  //       this match
  //         case Config(fields) =>
  //           fields.get(head) match
  //             case None => Null()
  //             case Some(f) => f.getValue(tail)
  //         case Arr(elems) =>
  //           head.toIntOption match
  //             case Some(idx) if 0 <= idx && idx < elems.size =>
  //               elems(idx).getValue(tail)
  //             case _ =>
  //               Null()
  //         case _ => Null()

  // def get[A](path: Path = Nil)(using reader: Reader[A]): A = reader.read(getValue(path), path.segments) match
  //   case Result.Success(a) => a
  //   case Result.Error(errors) =>
  //     val err = for e <- errors yield e.pretty + "\n"
  //     throw ReadException(err.mkString("\n", "\n", ""))

  def parse[A]()(using p: confuse.parsers.DerivationApi#Parser[A]): confuse.parsers.Result[A] =
    p.parse(this, Nil)

  def parseOrExit[A](
    exit: () => Nothing = () => sys.exit(1),
    stderr: java.io.PrintStream = System.err
  )(using p: confuse.parsers.DerivationApi#Parser[A]): A =
    parse[A]() match
      case confuse.parsers.Result.Success(a) => a
      case confuse.parsers.Result.FieldErrors(errs) =>
        for err <- errs do
          stderr.println(err.pretty)
        exit()




/** The null value. Indicates that a value has not been set or explicitly set to null. */
case class Null() extends Value with Terminal

/** A text value. */
case class Str(str: String) extends Value with Terminal

/** An array of configuration values. */
case class Arr(elems: mutable.ArrayBuffer[Value] = mutable.ArrayBuffer.empty) extends Value

// Note: merging and getting fields does not differentiate between unset and null values.
case class Config(fields: mutable.LinkedHashMap[String, Value] = mutable.LinkedHashMap.empty) extends Value:

  /** Merge the fields from the given object into this one.
    *
    * Note that only objects are merged. Any other values will overwrite
    * existing ones.
    */
  def mergeFrom(other: Config): Unit =
    for (k, v) <- other.fields do
      if !fields.contains(k) then
        fields += k -> v
      else
        val lhs = fields(k)
        (lhs, v) match
          case (o1: Config, o2: Config) =>
            o1.mergeFrom(o2)
            o1.origins = o2.origins ::: o1.origins
          case _ =>
            v.origins = v.origins ::: lhs.origins
            fields(k) = v

        // (fields(k), v) match
        //   case (o1: Config, o2: Config) =>
        //     o1.mergeFrom(o2)
        //   case (t1: Terminal, t2: Terminal) =>
        //     t2.origins = t2.origins ::: t1.origins
        //     fields(k) = v
        //   case _ =>
        // fields(k) = v
      end if

  def setValue(path: List[String], value: Value): Unit =
    path match
      case Nil => sys.error("empty key is not allowed")
      case key :: Nil =>
        (fields.get(key), value) match
          case (Some(t1: Terminal), t2: Terminal) =>
            t2.origins = t2.origins ::: t1.origins
          case _ =>
        fields(key) = value
      case head :: tail =>
        val next = fields.get(head) match
          case Some(o: Config) => o
          case _ =>
            val o = Config()
            fields(head) = o
            o
        next.setValue(tail, value)

  def setValue(path: String, value: Value): Unit =
    setValue(path.split('.').toList, value)

  def set(path: String, value: String, origin: Origin = null)(using here: SourcePos): Unit =
    val origin1 = if origin != null then origin else Origin.Code(here.path, here.row, here.col)
    val v = Str(value)
    v.origins = List(origin1)
    setValue(path, v)

  // def remove(path: String, origin: Origin = null)(using here: SourcePos): Unit =
  //   val origin1 = if origin != null then origin else Origin.Code(here.path, here.row, here.col)
  //   val v = Null()
  //   v.origins = List(origin1)
  //   setValue(path, v)

  // def unmarshal[A](using reader: confuse.api.ReaderApi#Reader[A]): A = reader.read(this, Nil) match
  //   case Result.Success(a) => a
  //   case Result.Error(errors) =>
  //       val err = for e <- errors yield e.pretty
  //       throw ReadException(err.mkString("\n", "\n", ""))


  // def unmarshalOrExit[A](
  //   stderr: java.io.PrintStream = System.err,
  //   exit: Int => Nothing = sys.exit
  // )(using reader: confuse.api.ReaderApi#Reader[A]): A =
  //   reader.read(this, Nil) match
  //     case Result.Success(a) => a
  //     case Result.Error(errs) =>
  //       for err <- errs do stderr.println(err.pretty)
  //       exit(1)

  // def get(path: List[String]): Value =
  //   path match
  //     case Nil => sys.error("empty key is not allowed")
  //     case head :: Nil =>
  //       fields.get(head) match
  //         case None => Null()
  //         case Some(v) => v
  //     case head :: tail =>
  //       fields.get(head) match
  //         case Some(o: Config) => o.get(tail)
  //         case Some(a: Arr) =>
  //           head.toIntOption match
  //             case Some(idx) if 0 <= idx && idx < a.elems.size =>
  //               a.elems(idx)
  //             case _ => Null()
  //         case _ => Null()

  // def get(key: String): Value = get(key.split('.').toList)

object Config:
  def apply(items: (String, Value)*): Config =
    val map = mutable.LinkedHashMap[String, Value]()
    for (i <- items) map.put(i._1, i._2)
    Config(map)
