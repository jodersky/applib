package confuse

import collection.mutable as m
import confuse.util.SourcePos


enum Origin:
  case File(file: String, row: Int, col: Int)
  case Env(name: String)
  case Props(name: String)
  case Arg()
  case Code(file: String, row: Int, col: Int)

  def pretty = this match
    case File(file, row, col) =>
      val b = StringBuilder()
      b ++= file
      if row >= 0 then
        b += ':'
        b ++= row.toString
      if col >= 0 then
        b += ':'
        b ++= col.toString
      b.result()
    case Env(name) =>
      s"env $name"
    case Props(name) =>
      s"system property $name"
    case Arg() =>
      s"arg"
    case Code(file, row, col) =>
      s"source $file:$row:$col"

/** Marker trait for terminal values.
  *
  * A terminal value does not contain other values.
  */
sealed trait Terminal extends Value

sealed trait Value:

  var origins: List[Origin] = Nil

  def flatten(path: Path = Nil): m.LinkedHashMap[String, Terminal] =
    val buffer = m.LinkedHashMap.empty[String, Terminal]
    flattenInto(path, buffer)
    buffer

  def flattenInto(path: Path = Nil, buffer: m.LinkedHashMap[String, Terminal]): Unit =
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

  def getValue(path: Path): Value =
    path.segments match
      case Nil => this
      case head :: tail =>
        this match
          case Config(fields) =>
            fields.get(head) match
              case None => Null()
              case Some(f) => f.getValue(tail)
          case Arr(elems) =>
            head.toIntOption match
              case Some(idx) if 0 <= idx && idx < elems.size =>
                elems(idx).getValue(tail)
              case _ =>
                Null()
          case _ => Null()

  // def get[A](path: Path = Nil)(using reader: Reader[A]): A = reader.read(getValue(path), path.segments) match
  //   case Result.Success(a) => a
  //   case Result.Error(errors) =>
  //     val err = for e <- errors yield e.pretty + "\n"
  //     throw ReadException(err.mkString("\n", "\n", ""))


/** The null value. Indicates that a value has not been set or explicitly set to null. */
case class Null() extends Value with Terminal

/** A text value. */
case class Str(str: String) extends Value with Terminal

/** An array of configuration values. */
case class Arr(elems: m.ArrayBuffer[Value] = m.ArrayBuffer.empty) extends Value

// Note: merging and getting fields does not differentiate between unset and null values.
case class Config(fields: m.LinkedHashMap[String, Value] = m.LinkedHashMap.empty) extends Value:

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
    val origin1 = if origin != null then origin else here.toOrigin
    val v = Str(value)
    v.origins = List(origin1)
    setValue(path, v)

  def remove(path: String, origin: Origin = null)(using here: SourcePos): Unit =
    val origin1 = if origin != null then origin else here.toOrigin
    val v = Null()
    v.origins = List(origin1)
    setValue(path, v)

  // def unmarshal[A](path: List[String] = Nil)(using reader: Reader[A]): A = reader.read(this, path) match
  //   case Result.Success(a) => a
  //   case Result.Error(errors) =>
  //       val err = for e <- errors yield e.pretty
  //       throw ReadException(err.mkString("\n", "\n", ""))



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
    val map = m.LinkedHashMap[String, Value]()
    for (i <- items) map.put(i._1, i._2)
    Config(map)
