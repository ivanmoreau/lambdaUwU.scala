import mill._, scalalib._, scalajslib._, mill.scalajslib.api._, scalanativelib._, scalanativelib.api._

object lambda extends ScalaJSModule {
  override def scalaVersion = "3.2.2"
  override def scalaJSVersion = "1.11.0"
  // override def ImoduleKind = T { ModuleKind.ESModule }
  override def moduleDeps = Seq(lambdaCoreJS)
  override def ivyDeps = Agg(
    ivy"com.raquo:laminar_sjs1_3:15.0.0-M4"
  )
}

trait lambdaDesktop extends CrossSbtModule {
  override def crossScalaVersion = "3.2.2"

  override def ivyDeps = Agg(ivy"org.typelevel::cats-core:2.9.0",
    ivy"org.typelevel::cats-effect::3.4.2",
    ivy"org.typelevel::cats-mtl::1.3.0",
    ivy"org.typelevel::kittens::3.0.0",
    ivy"org.typelevel::mouse::1.2.1",
    ivy"co.fs2::fs2-core::3.4.0",
    ivy"co.fs2::fs2-io::3.4.0",
  )

  override def millSourcePath = build.millSourcePath / "lamdaDesktop"
  override def sources = T.sources(
    millSourcePath / "src",
  )
}

object lambdaDesktopJVM extends lambdaDesktop with ScalaModule {
  override def moduleDeps = Seq(lambdaCoreJVM)
}

object lambdaDesktopNative extends lambdaDesktop with ScalaNativeModule {
  def scalaNativeVersion = "0.4.12"
  override def moduleDeps = Seq(lambdaCoreNative)
  def releaseMode = ReleaseMode.ReleaseFast
}

trait lambdaCore extends CrossSbtModule {
  override def crossScalaVersion = "3.2.2"
  // override def ImoduleKind = T { ModuleKind.ESModule }
  override def ivyDeps = Agg(ivy"org.typelevel::cats-core:2.9.0",
    ivy"org.typelevel::cats-effect::3.4.2",
    ivy"org.typelevel::cats-mtl::1.3.0",
    ivy"org.typelevel::kittens::3.0.0",
    ivy"org.typelevel::mouse::1.2.1",
    ivy"org.typelevel::cats-parse::0.3.7",
    ivy"co.fs2::fs2-core::3.4.0",
    ivy"co.fs2::fs2-io::3.4.0",
    ivy"org.typelevel::cats-parse::0.3.9"
  )

  override def millSourcePath = build.millSourcePath / "lambdaCore"
  override def sources = T.sources(
    millSourcePath / "src",
  )
}

object lambdaCoreJS extends lambdaCore with ScalaJSModule {
  override def scalaJSVersion = "1.11.0"
}

object lambdaCoreJVM extends lambdaCore with ScalaModule {
  object tests extends Tests {
    def ivyDeps = Agg(ivy"org.scalatest::scalatest:3.2.15",
      ivy"org.scalatestplus::scalacheck-1-17:3.2.15.0"
    )
    def testFramework = "org.scalatest.tools.Framework"

    override def sources = T.sources(
      millSourcePath / "test",
    )
  }
}

object lambdaCoreNative extends lambdaCore with ScalaNativeModule {
  def scalaNativeVersion = "0.4.12"
  def releaseMode = ReleaseMode.ReleaseFast
  def nativeLTO = LTO.Thin
}