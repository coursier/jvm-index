// using scala 2.13
// using com.softwaremill.sttp.client::core:2.0.0-RC6
// using com.lihaoyi::ujson:0.9.5
// using com.lihaoyi::os-lib:0.7.5
// using option -deprecation
// using option -Ywarn-unused

object GenerateIndex {

  def main(args: Array[String]): Unit = {

    val output = "index.json"

    val graalvmIndex0  = Graalvm.fullIndex(GhToken.token)
    val adoptIndex0    = Adopt.fullIndex(GhToken.token)
    val zuluIndex0     = Zulu.index()
    val libericaIndex0 = Liberica.index()

    val json = (graalvmIndex0 + adoptIndex0 + zuluIndex0 + libericaIndex0).json
    val dest = os.Path(output, os.pwd)
    os.write.over(dest, json)
    System.err.println(s"Wrote $dest")
  }
}
