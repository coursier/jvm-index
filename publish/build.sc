import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`

import de.tobiasroeser.mill.vcs.version.VcsVersion
import mill._
import mill.api.WorkspaceRoot
import mill.scalalib._
import mill.scalalib.publish._

import scala.concurrent.duration._

trait IndexModule extends Cross.Module[String] with PublishModule {
  def moduleName = s"index-$crossValue"
  def pomSettings = PomSettings(
    description = s"JVM index for $crossValue",
    organization = "io.get-coursier.jvm.indices",
    url = "https://github.com/coursier/jvm-index",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("io.get-coursier", "jvm-index"),
    developers = Seq(
      Developer("alexarchambault", "Alex Archambault", "https://github.com/alexarchambault")
    )
  )
  def publishVersion      = VcsVersion.vcsState().format()
  def sonatypeUri         = "https://s01.oss.sonatype.org/service/local"
  def sonatypeSnapshotUri = "https://s01.oss.sonatype.org/content/repositories/snapshots"

  def indexFile = T.source {
    PathRef(T.workspace / os.up / "indices" / s"$crossValue.json")
  }

  def indexResourceDir = T {
    val dest = T.dest
    os.copy.over(
      indexFile().path,
      dest / "coursier/jvm/indices/v1" / s"$crossValue.json",
      createFolders = true
    )
    PathRef(dest)
  }

  def resources = T.sources(
    Seq(indexResourceDir())
  )
}

lazy val osCpus =
  os.list(WorkspaceRoot.workspaceRoot / os.up / "indices")
    .filter(_.last.endsWith(".json"))
    .filter(os.isFile)
    .map(_.last.stripSuffix(".json"))

object index extends Cross[IndexModule](osCpus)

object ci extends Module {

  // same publishing stuff I copy-paste in all my projects 😬

  private def publishSonatype0(
    credentials: String,
    pgpPassword: String,
    data: Seq[PublishModule.PublishData],
    timeout: Duration,
    log: mill.api.Logger
  ): Unit = {

    val artifacts = data.map {
      case PublishModule.PublishData(a, s) =>
        (s.map { case (p, f) => (p.path, f) }, a)
    }

    val isRelease = {
      val versions = artifacts.map(_._2.version).toSet
      val set      = versions.map(!_.endsWith("-SNAPSHOT"))
      assert(
        set.size == 1,
        s"Found both snapshot and non-snapshot versions: ${versions.toVector.sorted.mkString(", ")}"
      )
      set.head
    }
    val publisher = new SonatypePublisher(
      uri = "https://s01.oss.sonatype.org/service/local",
      snapshotUri = "https://s01.oss.sonatype.org/content/repositories/snapshots",
      credentials = credentials,
      signed = isRelease,
      gpgArgs = Seq(
        "--passphrase",
        pgpPassword,
        "--no-tty",
        "--pinentry-mode",
        "loopback",
        "--batch",
        "--yes",
        "-a",
        "-b"
      ),
      readTimeout = timeout.toMillis.toInt,
      connectTimeout = timeout.toMillis.toInt,
      log = log,
      workspace = os.pwd,
      env = sys.env,
      awaitTimeout = timeout.toMillis.toInt,
      stagingRelease = isRelease
    )

    publisher.publishAll(isRelease, artifacts: _*)
  }

  def publishSonatype(tasks: mill.main.Tasks[PublishModule.PublishData]) =
    T.command {
      val timeout     = 10.minutes
      val credentials = sys.env("PUBLISH_USER") + ":" + sys.env("PUBLISH_PASSWORD")
      val pgpPassword = sys.env("PUBLISH_SECRET_KEY_PASSWORD")
      val data        = T.sequence(tasks.value)()

      publishSonatype0(
        credentials = credentials,
        pgpPassword = pgpPassword,
        data = data,
        timeout = timeout,
        log = T.ctx().log
      )
    }
}
