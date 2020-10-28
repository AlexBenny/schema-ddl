/**
 * Copyright (c) 2014-2017 Snowplow Analytics Ltd. All rights reserved.
 *
 * This program is licensed to you under the Apache License Version 2.0,
 * and you may not use this file except in compliance with the Apache License Version 2.0.
 * You may obtain a copy of the Apache License Version 2.0 at http://www.apache.org/licenses/LICENSE-2.0.
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the Apache License Version 2.0 is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the Apache License Version 2.0 for the specific language governing permissions and limitations there under.
 */

lazy val root = project.in(file("."))
  .settings(
    organization       := "com.snowplowanalytics",
    name               := "schema-ddl",
    version            := "0.12.0-M3",
    description        := "Set of Abstract Syntax Trees for various DDL and Schema formats",
    scalaVersion       := "2.12.12",
    crossScalaVersions := Seq("2.12.12", "2.13.3"),
    javacOptions       := BuildSettings.javaCompilerOptions
  )
  .enablePlugins(SiteScaladocPlugin)
  .settings(BuildSettings.sbtSiteSettings)
  .settings(BuildSettings.buildSettings)
  .settings(BuildSettings.scoverage)
  .settings(libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    Dependencies.Libraries.igluCoreJson4s,
    Dependencies.Libraries.igluCoreCirce,
    Dependencies.Libraries.circeGeneric,
    Dependencies.Libraries.circeLiteral,
    Dependencies.Libraries.circeParser,
    Dependencies.Libraries.jsonValidator,
    Dependencies.Libraries.libCompat,
    // Scala (test only)
    Dependencies.Libraries.specs2,
    Dependencies.Libraries.scalaCheck,
    Dependencies.Libraries.specs2Scalacheck,
    Dependencies.Libraries.specs2Cats
  ))
