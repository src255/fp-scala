Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := "0.0.1"

ThisBuild / scalaVersion := "2.13.8"

ThisBuild / organization := "fpWithScala"

triggeredMessage in ThisBuild := Watched.clearWhenTriggered

reColors := Revolver.noColors
