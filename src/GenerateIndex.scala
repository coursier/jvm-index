//> using scala "2.13"
//> using lib "com.softwaremill.sttp.client3::core:3.8.6"
//> using lib "com.lihaoyi::ujson:2.0.0"
//> using lib "com.lihaoyi::os-lib:0.9.0"
//> using option "-deprecation"
//> using option "-Ywarn-unused"

object GenerateIndex {

  def main(args: Array[String]): Unit = {

    val output = "index.json"

    val graalvmIndex0  = Graalvm.fullIndex(GhToken.token)
    val adoptIndex0    = Temurin.fullIndex(GhToken.token)
    val zuluIndex0     = Zulu.index()
    val libericaIndex0 = Liberica.index()

    val json = (graalvmIndex0 + adoptIndex0 + zuluIndex0 + libericaIndex0).json
    val dest = os.Path(output, os.pwd)
    os.write.over(dest, json)
    System.err.println(s"Wrote $dest")
  }
}
