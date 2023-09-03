package confuse

case class Path(segments: List[String] = Nil):
  override def toString(): String = segments.mkString(".")

object Path:
  given Conversion[List[String], Path] = ls => Path(ls)
  given Conversion[String, Path] = s => Path(s.split('.').toList)
