package clam.derivation2

import clam.completion.BashCompleter

trait CompletionApi:

  trait Completer[A]:
    def interactiveCompleter: String => Seq[String]
    def standaloneCompleter: BashCompleter


trait LowPrioCompleters extends CompletionApi:

  given emptyCompleter[A]: Completer[A] with
    val interactiveCompleter = _ => Seq()
    val standaloneCompleter = BashCompleter.Empty

trait StandardCompleters extends CompletionApi with LowPrioCompleters:

  private class FsCompleter[A]() extends Completer[A]:
    val standaloneCompleter = BashCompleter.Default

    val interactiveCompleter: String => Seq[String] = (prefix: String) =>
      import java.nio.file.{Files, Path, Paths}

      try
        val completions = collection.mutable.ListBuffer.empty[String]
        val path = Paths.get(prefix)

        def addListing(dir: Path) =
          val children = Files.list(dir).iterator()
          while (children.hasNext())
            val path = children.next()
            if path.toString.startsWith(prefix) then
              if Files.isDirectory(path) then
                completions += s"$path/"
              else
                completions += s"$path "

        if (Files.isDirectory(path) && prefix.endsWith("/"))
          addListing(path)
        else
          path.getParent() match
            case null => addListing(Paths.get(""))
            case dir  => addListing(dir)
        completions.result()
      catch
        case _: Exception => Seq()


  given filePathCompleter: Completer[os.FilePath] = FsCompleter[os.FilePath]()
  given absPathCompleter: Completer[os.Path] = FsCompleter[os.Path]()
  given relPathCompleter: Completer[os.RelPath] = FsCompleter[os.RelPath]()
  given subPathCompleter: Completer[os.SubPath] = FsCompleter[os.SubPath]()

  given javaFileCompleter: Completer[java.io.File] = FsCompleter[java.io.File]()
  given javaPathCompleter: Completer[java.nio.file.Path] = FsCompleter[java.nio.file.Path]()

  given booleanCompleter: Completer[Boolean] with
    val interactiveCompleter =
      prefix => Seq("true", "false").filter(_.startsWith(prefix))
    val standaloneCompleter = BashCompleter.Fixed(Set("true", "false"))

