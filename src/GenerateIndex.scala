//> using scala 3
//> using dep com.softwaremill.sttp.client3::core:3.9.0
//> using dep com.lihaoyi::ujson:3.1.3
//> using dep com.lihaoyi::os-lib:0.9.1
//> using options -Wunused:all -deprecation

object GenerateIndex {

  def main(args: Array[String]): Unit = {

    val baseName = "index"

    val dest = os.pwd / s"$baseName.json"

    val correttoIndex0      = Corretto.fullIndex(GhToken.token)
    val graalvmLegacyIndex0 = GraalvmLegacy.fullIndex(GhToken.token)
    val graalvmIndex0       = Graalvm.fullIndex(GhToken.token)
    val oracleIndex0        = Oracle.index()
    val adoptIndex0         = Temurin.fullIndex(GhToken.token)
    val zuluIndex0          = Zulu.index()
    val libericaIndex0      = Liberica.index()

    val index = graalvmLegacyIndex0 +
      graalvmIndex0 +
      oracleIndex0 +
      adoptIndex0 +
      zuluIndex0 +
      libericaIndex0 +
      correttoIndex0

    val json = index.json
    os.write.over(dest, json)
    System.err.println(s"Wrote $dest")

    for (((os0, arch), osArchIndex) <- index.osArchIndices.toVector.sortBy(_._1)) {
      val dest0 = os.pwd / "indices" / s"$os0-$arch.json"
      val json0 = osArchIndex.json
      os.write.over(dest0, json0, createFolders = true)
      System.err.println(s"Wrote $dest0")
    }
  }
}
