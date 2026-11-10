//> using scala 3.8.1
//> using jvm 21
//> using dep com.softwaremill.sttp.client3::core:3.11.0
//> using dep com.lihaoyi::ujson:4.4.3
//> using dep com.lihaoyi::os-lib:0.11.8
//> using dep com.lihaoyi::pprint:0.9.6
//> using dep io.get-coursier:versions_2.13:0.5.1
//> using dep io.get-coursier:interface:1.0.29-M3
//> using dep org.jsoup:jsoup:1.22.1
//> using options -Wunused:all -deprecation -Werror

package coursier.jvmindex

import java.util.concurrent.Executors

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

object GenerateIndex {

  def main(args: Array[String]): Unit = {

    val baseName = "index"

    val dest = os.sub / s"$baseName.json"

    val indicesV2Dir = os.sub / "indices-v2"

    // Checksums of archives already in the v2 indices, so that we don't download
    // their checksum files again
    val knownChecksums = Checksums.fromV2Indices(os.pwd / indicesV2Dir)
    System.err.println(s"Read ${knownChecksums.size} checksums from $indicesV2Dir")

    val pool = Executors.newFixedThreadPool(6)

    val (index0, warnings) =
      try {
        implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)

        val mandrelFuture = Future(Mandrel.fullIndex(GhToken.token))

        val futures = Seq(
          Future(Corretto.fullIndex(GhToken.token)),
          Future(GraalvmLegacy.fullIndex(GhToken.token)),
          Future(Graalvm.fullIndex(GhToken.token)),
          Future(Oracle.index()),
          Future(Temurin.fullIndex(GhToken.token)),
          Future(Zulu.index()),
          Future(Liberica.index()),
          Future(LibericaNik.index()),
          Future(IbmSemeru.fullIndex(GhToken.token)),
          Future(Microsoft.fullIndex()),
          Future(JetBrains.fullIndex(GhToken.token)),
          mandrelFuture.map(_._1)
        )

        val index = futures
          .map(f => Await.result(f, Duration.Inf))
          .foldLeft(Index.empty)(_ + _)
        val warnings = Await.result(mandrelFuture, Duration.Inf)._2
        (index, warnings)
      }
      finally
        pool.shutdown()

    // Warnings about things that need to be looked at, but that shouldn't prevent the
    // index from being updated. The index update workflow posts them on its pull request.
    for (warningsFile <- Option(System.getenv("INDEX_WARNINGS_FILE")).filter(_.nonEmpty)) {
      val content = warnings.map(warning => s"- $warning${System.lineSeparator()}").mkString
      os.write.over(os.Path(warningsFile, os.pwd), content, createFolders = true)
      System.err.println(s"Wrote ${warnings.length} warning(s) to $warningsFile")
    }

    val index = Checksums.resolve(index0, knownChecksums)

    val json = index.json
    os.write.over(os.pwd / dest, json)
    System.err.println(s"Wrote $dest")

    val indicesDir = os.sub / "indices"

    System.err.println(s"Removed $indicesDir")
    os.remove.all(os.pwd / indicesDir)
    System.err.println(s"Removed $indicesV2Dir")
    os.remove.all(os.pwd / indicesV2Dir)
    for (((os0, arch), osArchIndex) <- index.osArchIndices.toVector.sortBy(_._1)) {
      val dest0 = indicesDir / s"$os0-$arch.json"
      val json0 = osArchIndex.json
      os.write.over(os.pwd / dest0, json0, createFolders = true)
      System.err.println(s"Wrote $dest0")

      val destV2 = indicesV2Dir / s"$os0-$arch.json"
      val jsonV2 = osArchIndex.jsonV2
      os.write.over(os.pwd / destV2, jsonV2, createFolders = true)
      System.err.println(s"Wrote $destV2")
    }
  }
}
