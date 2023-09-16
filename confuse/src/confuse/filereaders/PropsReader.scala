package confuse.filereaders

import confuse.Config
import confuse.FileReader
import confuse.FileReader.Result
import confuse.Origin
import confuse.Value
import confuse.FileReader.Result

object PropsReader extends FileReader:

  override def read(file: String, stream: java.io.InputStream, sizeHint: Int): Result =
    import scala.jdk.CollectionConverters.*
    val props = java.util.Properties()
    props.load(stream)

    val cfg = Config()
    for name <- props.stringPropertyNames().asScala do
      val value = props.getProperty(name)
      if value != null then
        cfg.set(name, value, Origin.File(file, -1, -1)) // TODO: this reader doesn't support location information

    Result.Success(cfg)
