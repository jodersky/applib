import mill._, scalalib._, scalanativelib._, scalafmt._, publish._

val scala3 = "3.2.2"
val scalaNative = "0.4.14"

trait Publish extends PublishModule {
  def publishVersion = "0.2.1" // gitVersion()
  def pomSettings = PomSettings(
    description = "applib",
    organization = "io.crashbox",
    url = "https://github.com/jodersky/applib",
    licenses = Seq(License.MIT),
    versionControl = VersionControl.github("jodersky", "applib"),
    developers = Seq(
      Developer("jodersky", "Jakob Odersky", "https://github.com/jodersky")
    )
  )
}

trait Utest extends TestModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.8.1",
    ivy"com.lihaoyi::upickle:3.0.0"
  )
  def testFramework = "utest.runner.Framework"
}

// Command Line Argument Mapper
object clam extends Module {
  trait ClamModule extends ScalaModule with ScalafmtModule with Publish {
    def scalaVersion = scala3
    def scalacOptions = Seq("-deprecation", "-release", "8")
    def millSourcePath = super.millSourcePath / os.up
    def artifactName = "clam"
  }

  object jvm extends ClamModule {
    def moduleDeps = Seq(`clam-core`.jvm)
    object test extends ScalaTests with Utest
  }
  object native extends ClamModule with ScalaNativeModule{
    def scalaNativeVersion = scalaNative
    def moduleDeps = Seq(`clam-core`.native)
    object test extends ScalaTests with Utest
  }
  object example extends ScalaModule {
    def scalaVersion = scala3
    def moduleDeps = Seq(clam.jvm)
  }
}

// Core pieces of code that are depended on by other projects and aren't
// strictly related to clam's functionality. E.g. string readers that are shared
// between clam and confuse.
object `clam-core` extends Module {
  trait ClamModule extends ScalaModule with ScalafmtModule with Publish {
    def scalaVersion = scala3
    def scalacOptions = Seq("-deprecation", "-release", "8")
    def millSourcePath = super.millSourcePath / os.up
    def artifactName = "clam-core"
    def ivyDeps = Agg(
      ivy"com.lihaoyi::os-lib::0.9.1"
    )
  }

  object jvm extends ClamModule {
    object test extends ScalaTests with Utest
  }
  object native extends ClamModule with ScalaNativeModule{
    def scalaNativeVersion = scalaNative
    object test extends ScalaTests with Utest
  }
}

// Configuration parsing for multiple formats
object confuse extends Module {
  trait ConfuseModule extends ScalaModule with ScalafmtModule with Publish {
    def scalaVersion = scala3
    def scalacOptions = Seq("-deprecation", "-release", "8")
    def ivyDeps = Agg(
      ivy"com.lihaoyi::os-lib::0.9.1",
      ivy"com.lihaoyi::ujson::3.1.0",
      ivy"io.crashbox::yamlesque::0.3.2",
      ivy"org.ekrich::sconfig::1.5.0"
    )
    def millSourcePath = super.millSourcePath / os.up
    def artifactName = "confuse"
  }
  object jvm extends ConfuseModule {
    def moduleDeps = Seq(`clam-core`.jvm)
    def sources = T.sources(super.sources() ++ Seq(PathRef(millSourcePath / "src-jvm")))
    object test extends ScalaTests with Utest
  }
  object native extends ConfuseModule with ScalaNativeModule {
    def scalaNativeVersion = scalaNative
    def moduleDeps = Seq(`clam-core`.native)
    def sources = T.sources(super.sources() ++ Seq(PathRef(millSourcePath / "src-native")))
    def ivyDeps = super.ivyDeps() ++ Agg(
      ivy"org.ekrich::sjavatime::1.1.9",
    )
    def nativeLinkStubs = true
    object test extends ScalaTests with Utest {
      def nativeLinkStubs = true
    }
  }
  object example extends ScalaModule {
    def scalaVersion = scala3
    def moduleDeps = Seq(confuse.jvm)
  }
}

object sandbox extends Module {
  trait SandboxModule extends ScalaModule {
    def scalaVersion = scala3
    def millSourcePath = super.millSourcePath / os.up
  }
  object jvm extends SandboxModule {
    def moduleDeps = Seq(clam.jvm, confuse.jvm)
  }
  object native extends SandboxModule with ScalaNativeModule {
    def moduleDeps = Seq(clam.native, confuse.native)
    def scalaNativeVersion = scalaNative
    def nativeLinkStubs = true
  }
}
