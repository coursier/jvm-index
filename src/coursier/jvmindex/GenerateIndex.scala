//> using scala 3.8.1
//> using jvm 21
//> using dep com.softwaremill.sttp.client3::core:3.10.1
//> using dep com.lihaoyi::ujson:4.0.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep com.lihaoyi::pprint:0.9.0
//> using dep io.get-coursier:versions_2.13:0.3.3
//> using dep io.get-coursier:interface:1.0.29-M3
//> using dep org.jsoup:jsoup:1.20.1
//> using options -Wunused:all -deprecation

package coursier.jvmindex

import java.util.concurrent.Executors

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

object GenerateIndex {

  def main(args: Array[String]): Unit = {

    val baseName = "index"

    val dest = os.sub / s"$baseName.json"

    val pool = Executors.newFixedThreadPool(6)

    val index =
      try {
        implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(pool)

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
          Future(Microsoft.fullIndex())
        )

        futures
          .map(f => Await.result(f, Duration.Inf))
          .foldLeft(Index.empty)(_ + _)
      }
      finally
        pool.shutdown()

    val json = index.json
    os.write.over(os.pwd / dest, json)
    System.err.println(s"Wrote $dest")

    val indicesDir = os.sub / "indices"

    System.err.println(s"Removed $indicesDir")
    os.remove.all(os.pwd / indicesDir)
    for (((os0, arch), osArchIndex) <- index.osArchIndices.toVector.sortBy(_._1)) {
      val dest0 = indicesDir / s"$os0-$arch.json"
      val json0 = osArchIndex.json
      os.write.over(os.pwd / dest0, json0, createFolders = true)
      System.err.println(s"Wrote $dest0")
    }
  }
}
