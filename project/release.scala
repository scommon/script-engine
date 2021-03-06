import sbt._
import Keys._

import sbtrelease._
import ReleaseStateTransformations._
import ReleasePlugin._
import ReleaseKeys._

object ReleaseSettings {
  val defaults = releaseSettings ++ Seq(
      //Customize the steps of the release process.
      releaseProcess := Seq[ReleaseStep](
        checkSnapshotDependencies              //
      , runTest                                //
      , inquireVersions                        //
      , setReleaseVersion                      //
      , commitReleaseVersion                   // performs the initial git checks
      , tagRelease                             //
      //, publishArtifacts                       //checks whether `publishTo` is properly set up
      , setNextVersion                         //
      , commitNextVersion                      //
      , pushChanges                            //also checks that an upstream branch is properly configured
    ),
      //Customize the next version string to bump the revision number.
      nextVersion := { ver =>
        Version(ver)
          .map(_.bumpBugfix.asSnapshot.string)
          .getOrElse(versionFormatError)
      }
  )
}

