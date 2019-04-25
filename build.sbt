/**
  * fol2asp
  *
  * Copyright (c) 2016-2019 Matthias Nickles
  *
  * https://www.researchgate.net/profile/Matthias_Nickles
  *
  * License: https://www.apache.org/licenses/LICENSE-2.0
  *
  */

name := "fol2asp"

version := "0.4.4"

scalaVersion := "2.12.8"

mainClass in (Compile, run) := Some("commandline.fol2asp")

mainClass in (Compile, packageBin) := Some("commandline.fol2asp")

