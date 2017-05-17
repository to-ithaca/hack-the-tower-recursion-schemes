lazy val compilerSettings = Seq(
  scalacOptions ++= Seq(
    "-language:higherKinds",
    "-Ypartial-unification",
    "-Yliteral-types",
    "-encoding", "UTF-8",
    "-language:implicitConversions",
    "-language:postfixOps",
    "-language:existentials"
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  addCompilerPlugin("com.github.mpilquist" %% "simulacrum" % "0.10.0")
)

lazy val commonResolvers = Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  Resolver.jcenterRepo
)

lazy val buildSettings = Seq(
  scalaOrganization := "org.typelevel",
  scalaVersion := "2.12.0",
  name := "hack-the-tower-recursion-schemes",
  version := "0.1.0-SNAPSHOT"
)

lazy val catsVersion = "0.9.0"

lazy val commonSettings = Seq(
  resolvers := commonResolvers,
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.typelevel" %% "cats-free" % catsVersion,
    "com.chuusai" %% "shapeless" % "2.3.2",
    "com.slamdata" %% "matryoshka-core" % "0.18.3",
    "org.scalatest" %% "scalatest" % "3.0.1" % "test",
    "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
  )
) ++ compilerSettings

lazy val root = (project in file(".")).settings(
  buildSettings,
  commonSettings
)
