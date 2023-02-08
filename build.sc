import mill._, scalalib._, scalajslib._, mill.scalajslib.api._

object lambda extends ScalaJSModule {
  override def scalaVersion = "3.2.1"
  override def scalaJSVersion = "1.11.0"
  override def ammoniteVersion = "2.5.5-17-df243e14"
  // override def ImoduleKind = T { ModuleKind.ESModule }
  override def ivyDeps = Agg(ivy"org.typelevel::cats-core:2.9.0",
    ivy"org.typelevel::cats-effect:3.4.2",
    ivy"org.typelevel::cats-mtl:1.3.0",
    ivy"org.typelevel::kittens:3.0.0",
    ivy"org.typelevel::mouse:1.2.1",
    ivy"org.typelevel::cats-parse:0.3.7",
    ivy"co.fs2::fs2-core:3.4.0",
    ivy"co.fs2::fs2-io:3.4.0",
    ivy"com.raquo:laminar_sjs1_3:15.0.0-M4",
    ivy"org.parboiled::parboiled::2.4.1",
    ivy"dev.zio::izumi-reflect::2.2.5"
  )
}
