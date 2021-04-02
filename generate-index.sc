
import $ivy.`com.softwaremill.sttp.client::core:2.0.0-RC6`
import $ivy.`com.lihaoyi::ujson:0.9.5`

import java.nio.file.{Files, Paths}

import sttp.client.quick._

import scala.util.control.NonFatal

final case class Index(map: Map[String, Map[String, Map[String, Map[String, String]]]]) {

  private def merge4(
    a: Map[String, Map[String, Map[String, Map[String, String]]]],
    b: Map[String, Map[String, Map[String, Map[String, String]]]]
  ): Map[String, Map[String, Map[String, Map[String, String]]]] =
    (a.keySet ++ b.keySet)
      .iterator
      .map { key =>
        val m = (a.get(key), b.get(key)) match {
          case (Some(a0), Some(b0)) =>
            merge3(a0, b0)
          case (Some(a0), None) =>
            a0
          case (None, Some(b0)) =>
            b0
          case (None, None) =>
            sys.error("cannot happen")
        }
        key -> m
      }
      .toMap

  private def merge3(
    a: Map[String, Map[String, Map[String, String]]],
    b: Map[String, Map[String, Map[String, String]]]
  ): Map[String, Map[String, Map[String, String]]] =
    (a.keySet ++ b.keySet)
      .iterator
      .map { key =>
        val m = (a.get(key), b.get(key)) match {
          case (Some(a0), Some(b0)) =>
            merge2(a0, b0)
          case (Some(a0), None) =>
            a0
          case (None, Some(b0)) =>
            b0
          case (None, None) =>
            sys.error("cannot happen")
        }
        key -> m
      }
      .toMap

  private def merge2(
    a: Map[String, Map[String, String]],
    b: Map[String, Map[String, String]]
  ): Map[String, Map[String, String]] =
    (a.keySet ++ b.keySet)
      .iterator
      .map { key =>
        val m = (a.get(key), b.get(key)) match {
          case (Some(a0), Some(b0)) =>
            merge1(a0, b0)
          case (Some(a0), None) =>
            a0
          case (None, Some(b0)) =>
            b0
          case (None, None) =>
            sys.error("cannot happen")
        }
        key -> m
      }
      .toMap

  private def merge1(
    a: Map[String, String],
    b: Map[String, String]
  ): Map[String, String] =
    (a.keySet ++ b.keySet)
      .iterator
      .map { key =>
        val m = (a.get(key), b.get(key)) match {
          case (Some(_), Some(b0)) =>
            b0 // keeping value from the map on the right
          case (Some(a0), None) =>
            a0
          case (None, Some(b0)) =>
            b0
          case (None, None) =>
            sys.error("cannot happen")
        }
        key -> m
      }
      .toMap

  def +(other: Index): Index =
    Index(merge4(map, other.map))

  import ujson._

  private def json4(
    map: Map[String, Map[String, Map[String, Map[String, String]]]]
  ) = {
    val l = map
      .toVector
      .sortBy(_._1)
      .map {
        case (k, m) =>
          k -> json3(m)
      }
    if (l.isEmpty)
      Js.Obj()
    else
      Js.Obj(l.head, l.tail: _*)
  }

  private def json3(
    map: Map[String, Map[String, Map[String, String]]]
  ) = {
    val l = map
      .toVector
      .sortBy(_._1)
      .map {
        case (k, m) =>
          k -> json2(m)
      }
    if (l.isEmpty)
      Js.Obj()
    else
      Js.Obj(l.head, l.tail: _*)
  }

  private def json2(
    map: Map[String, Map[String, String]]
  ) = {
    val l = map
      .toVector
      .sortBy(_._1)
      .map {
        case (k, m) =>
          k -> json1(m)
      }
    if (l.isEmpty)
      Js.Obj()
    else
      Js.Obj(l.head, l.tail: _*)
  }

  private def json1(
    map: Map[String, String]
  ) = {
    val l = map
      .toVector
      .sortBy(_._1)
      .map {
        case (k, v) =>
          k -> Js.Str(v)
      }
    if (l.isEmpty)
      Js.Obj()
    else
      Js.Obj(l.head, l.tail: _*)
  }

  def json: String =
    json4(map).render(indent = 2)
}

object Index {
  def empty: Index =
    Index(Map.empty)
  def apply(os: String, architecture: String, jdkName: String, jdkVersion: String, url: String): Index =
      Index(Map(os -> Map(architecture -> Map(jdkName -> Map(jdkVersion -> url)))))
}


final case class Release(
  releaseId: Long,
  tagName: String,
  prerelease: Boolean
)

final case class Asset(
  name: String,
  downloadUrl: String
)

def releaseIds(
  ghOrg: String,
  ghProj: String,
  ghToken: String
): Iterator[Release] = {

  def helper(page: Int): Iterator[Release] = {
    val url = uri"https://api.github.com/repos/$ghOrg/$ghProj/releases?page=$page"
    System.err.println(s"Getting $url")
    val resp = quickRequest.header("Authorization", s"token $ghToken").get(url).send()
    val linkHeader = resp.header("Link")
    val hasNext = linkHeader
      .toSeq
      .flatMap(_.split(','))
      .exists(_.endsWith("; rel=\"next\""))
    val json = ujson.read(resp.body)

    val res = try {
      json.arr.toVector.map { obj =>
        Release(obj("id").num.toLong, obj("tag_name").str, obj("prerelease").bool)
      }
    } catch {
      case NonFatal(e) =>
        System.err.println(resp.body)
        throw e
    }

    if (hasNext)
      res.iterator ++ helper(page + 1)
    else
      res.iterator
  }

  helper(1)
}

def releaseAssets(
  ghOrg: String,
  ghProj: String,
  ghToken: String,
  releaseId: Long
): Iterator[Asset] = {

  def helper(page: Int): Iterator[Asset] = {
    val url = uri"https://api.github.com/repos/$ghOrg/$ghProj/releases/$releaseId/assets?page=$page"
    System.err.println(s"Getting $url")
    val resp = quickRequest.header("Authorization", s"token $ghToken").get(url).send()
    val json = ujson.read(resp.body)

    val linkHeader = resp.header("Link")
    val hasNext = linkHeader
      .toSeq
      .flatMap(_.split(','))
      .exists(_.endsWith("; rel=\"next\""))

    val res = try {
      json.arr.toVector.map { obj =>
        Asset(obj("name").str, obj("browser_download_url").str)
      }
    } catch {
      case NonFatal(e) =>
        System.err.println(resp.body)
        throw e
    }

    if (hasNext)
      res.iterator ++ helper(page + 1)
    else
      res.iterator
  }

  helper(1)
}


def graalvmIndex(ghToken: String, javaVersion: String, javaVersionInName: java.lang.Boolean = null): Index = {

  val javaVersionInName0 = Option(javaVersionInName)
    .map(x => x: Boolean)
    .getOrElse(javaVersion != "8")
  val name =
    if (javaVersionInName0)
      s"jdk@graalvm-java$javaVersion"
    else
      "jdk@graalvm"

  val ghOrg = "graalvm"
  val ghProj = "graalvm-ce-builds"
  val releases0 = releaseIds(ghOrg, ghProj, ghToken)
    .filter(!_.prerelease)

  val assetNamePrefix = s"graalvm-ce-java${javaVersion}-"

  def osOpt(input: String): Option[(String, String)] =
    if (input.startsWith("linux-"))
      Some(("linux", input.stripPrefix("linux-")))
    else if (input.startsWith("darwin-"))
      Some(("darwin", input.stripPrefix("darwin-")))
    else if (input.startsWith("windows-"))
      Some(("windows", input.stripPrefix("windows-")))
    else
      None

  def archOpt(input: String): Option[(String, String)] =
    if (input.startsWith("amd64-"))
      Some(("amd64", input.stripPrefix("amd64-")))
    else if (input.startsWith("aarch64-"))
      Some(("arm64", input.stripPrefix("aarch64-")))
    else
      None

  def archiveTypeOpt(input: String): Option[String] =
    if (input == "zip") Some("zip")
    else if (input == "tar.gz") Some("tgz")
    else None

  val indices = releases0
    .filter(release => release.tagName.startsWith("vm-"))
    .flatMap { release =>
      val version = release.tagName.stripPrefix("vm-")
      val assets = releaseAssets(ghOrg, ghProj, ghToken, release.releaseId)
      assets
        .filter(asset => asset.name.startsWith(assetNamePrefix))
        .flatMap { asset =>
          val name0 = asset.name.stripPrefix(assetNamePrefix)
          val opt = for {
            (os, rem) <- osOpt(name0)
            (arch, rem0) <- archOpt(rem)
            ext <- Some(rem0)
              .filter(_.startsWith(version + "."))
              .map(_.stripPrefix(version + "."))
            archiveType <- archiveTypeOpt(ext)
          } yield Index(os, arch, name, version, archiveType + "+" + asset.downloadUrl)
          opt.toSeq
        }
    }

  indices.foldLeft(Index.empty)(_ + _)
}

def adoptIndex(
  ghToken: String,
  baseVersion: Int,
  versionPrefix: String = ""
): Index = {
  val ghOrg = "AdoptOpenJDK"
  val ghProj = s"openjdk$baseVersion-binaries"
  val releases0 = releaseIds(ghOrg, ghProj, ghToken)
    .filter(!_.prerelease)

  val releaseJdkName = "jdk@adopt"
  val releaseAssetNamePrefix = {
    val jdkStr = "jdk"
    if (baseVersion <= 15) s"OpenJDK${baseVersion}U-${jdkStr}_"
    else s"OpenJDK${baseVersion}-${jdkStr}_"
  }

  val debugJdkName = "jdk@adopt-debugimage"
  val debugAssetNamePrefix = {
    val jdkStr = "debugimage"
    if (baseVersion <= 15) s"OpenJDK${baseVersion}U-${jdkStr}_"
    else s"OpenJDK${baseVersion}-${jdkStr}_"
  }

  val testJdkName = "jdk@adopt-testimage"
  val testAssetNamePrefix = {
    val jdkStr = "testimage"
    if (baseVersion <= 15) s"OpenJDK${baseVersion}U-${jdkStr}_"
    else s"OpenJDK${baseVersion}-${jdkStr}_"
  }

  val jreName = "jdk@adopt-jre"
  val jreAssetNamePrefix = {
    val jdkStr = "jre"
    if (baseVersion <= 15) s"OpenJDK${baseVersion}U-${jdkStr}_"
    else s"OpenJDK${baseVersion}-${jdkStr}_"
  }

  def archOpt(input: String): Option[(String, String)] =
    if (input.startsWith("x64_"))
      Some(("amd64", input.stripPrefix("x64_")))
    else if (input.startsWith("x86-32_"))
      Some(("x86", input.stripPrefix("x86-32_")))
    else if (input.startsWith("aarch64_"))
      Some(("arm64", input.stripPrefix("aarch64_")))
    else if (input.startsWith("arm_"))
      Some(("arm", input.stripPrefix("arm_")))
    else if (input.startsWith("s390x_"))
      Some(("s390x", input.stripPrefix("s390x_")))
    else if (input.startsWith("ppc64_"))
      Some(("ppc64", input.stripPrefix("ppc64_")))
    else if (input.startsWith("ppc64le_"))
      Some(("ppc64le", input.stripPrefix("ppc64le_")))
    else
      None

  def osOpt(input: String): Option[(String, String)] =
    if (input.startsWith("linux_"))
      Some(("linux", input.stripPrefix("linux_")))
    else if (input.startsWith("alpine-linux_"))
      Some(("alpine-linux", input.stripPrefix("alpine-linux_")))
    else if (input.startsWith("mac_"))
      Some(("darwin", input.stripPrefix("mac_")))
    else if (input.startsWith("windows_"))
      Some(("windows", input.stripPrefix("windows_")))
    else if (input.startsWith("aix_"))
      Some(("aix", input.stripPrefix("aix_")))
    else
      None

  def archiveTypeOpt(input: String): Option[String] =
    if (input == "zip") Some("zip")
    else if (input == "tar.gz") Some("tgz")
    else None

  val prefixes =
    if (baseVersion == 8) Seq("jdk8u")
    else Seq(s"jdk-$baseVersion.", s"jdk-$baseVersion+")
  val indices = releases0
    .filter { release =>
      prefixes.exists(prefix => release.tagName.startsWith(prefix))
    }
    .flatMap { release =>
      val version0 = release.tagName.stripPrefix("jdk-").stripPrefix("jdk")
      val versionInFileName = {
        if (version0.contains("+"))
          version0.split('+') match {
            case Array(before, after) => s"${before}_${after.takeWhile(_ != '.')}"
            case _ => version0
          }
        else version0
      }
      val assets = releaseAssets(ghOrg, ghProj, ghToken, release.releaseId).toStream
      def index(jdkName: String, assetNamePrefix: String) = assets
        .iterator
        .filter(asset => asset.name.startsWith(assetNamePrefix))
        .flatMap { asset =>
          val name0 = asset.name.stripPrefix(assetNamePrefix)
          val opt = for {
            (arch, rem) <- archOpt(name0)
            (os, rem0) <- osOpt(rem)
            ext <- {
              val prefix = "hotspot_" + versionInFileName.filter(_ != '-') + "."
              Some(rem0)
                .filter(_.startsWith(prefix))
                .map(_.stripPrefix(prefix))
            }
            archiveType <- archiveTypeOpt(ext)
          } yield Index(os, arch, jdkName, "1." + version0.takeWhile(c => c != '-' && c != '+' && c != '_').replaceAllLiterally("u", ".0-"), archiveType + "+" + asset.downloadUrl)
          opt.toSeq
        }
      def releaseIndex = index(releaseJdkName, releaseAssetNamePrefix)
      def debugIndex = index(debugJdkName, debugAssetNamePrefix)
      def testIndex = index(testJdkName, testAssetNamePrefix)
      def jreIndex = index(jreName, jreAssetNamePrefix)
      releaseIndex ++ debugIndex ++ testIndex ++ jreIndex
    }

  indices.foldLeft(Index.empty)(_ + _)
}

final case class ZuluParams(
  indexOs: String,
  indexArch: String,
  indexArchiveType: String,
  bundleType: String = "jdk",
  releaseStatus: String = "ga"
) {
  lazy val url: sttp.model.Uri =
    uri"https://api.azul.com/zulu/download/community/v1.0/bundles/?os=$os&arch=$arch&hw_bitness=$bitness&bundle_type=$bundleType&ext=$ext&release_status=$releaseStatus&javafx=false"

  lazy val os = indexOs match {
    case "linux-musl" => "linux_musl"
    case "darwin"     => "macos"
    case x            => x
  }

  lazy val (arch, bitness) = indexArch match {
    case "arm"   => ("arm", "32")
    case "arm64" => ("arm", "64")
    case "x86"   => ("x86", "32")
    case "amd64" => ("x86", "64")
    case "ppc64" => ("ppc", "64")
    case _       => ???
  }

  lazy val ext = indexArchiveType match {
    case "tgz" => "tar.gz"
    case x     => x
  }

  lazy val indexJdkName = bundleType match {
    case "jdk" =>  "jdk@zulu"
    case x     => s"jdk@zulu-$x"
  }

  def index(jdkVersion: Seq[Int], url: String): Index = {
    val indexUrl = s"$indexArchiveType+$url"
    Index(indexOs, indexArch, indexJdkName, jdkVersion.take(3).mkString("."), indexUrl)
  }
}

def zuluIndex(): Index = {

  val oses = Seq("darwin", "linux", "windows", "linux-musl") // Add "solaris", "qnx"?
  val cpus = Seq("x86", "amd64", "arm", "arm64", "ppc64")
  val bundleTypes = Seq("jdk", "jre")
  val allParams = for {
    os <- oses
    cpu <- cpus
    ext = if (os == "windows") "zip" else "tgz"
    bundleType <- bundleTypes
  } yield ZuluParams(os, cpu, ext, bundleType = bundleType)

  allParams
    .flatMap { params =>
      System.err.println(s"Getting ${params.url}")
      val resp = quickRequest.get(params.url).send()
      val json = ujson.read(resp.body)

      val count = json.arr.length
      System.err.println(s"Found $count elements")

      json.arr
        .toArray
        .map(_.obj)
        .map(a => a("id").num.toInt -> a)
        .sortBy(_._1)
        .iterator
        .map(_._2)
        .map { obj =>
          val url = obj("url").str
          val jdkVersion = obj("jdk_version").arr.toList.map(_.num.toInt)
          params.index(jdkVersion, url)
        }
    }
    .foldLeft(Index.empty)(_ + _)
}

final case class LibericaParams(
  indexOs: String,
  indexArch: String,
  indexArchiveType: String,
  bundleType: String = "jdk"
) {
  lazy val url: sttp.model.Uri = {
    val fx = if (bundleType.endsWith("-full")) "true" else "false"
    uri"https://api.bell-sw.com/v1/liberica/releases?bitness=$bitness&fx=$fx&os=$os&arch=$arch&package-type=$ext&bundle-type=$bundleType"
  }

  lazy val os = indexOs match {
    case "darwin"     => "macos"
    case x            => x
  }

  lazy val (arch, bitness) = indexArch match {
    case "arm"   => ("arm", "32")
    case "arm64" => ("arm", "64")
    case "x86"   => ("x86", "32")
    case "amd64" => ("x86", "64")
    case "ppc64" => ("ppc", "64")
    case _       => ???
  }

  lazy val ext = indexArchiveType match {
    case "tgz" => "tar.gz"
    case x     => x
  }

  lazy val indexJdkName = bundleType match {
    case "jdk"                         =>  "jdk@liberica"
    case jdk if jdk.startsWith("jdk-") =>  "jdk@liberica" + jdk.stripPrefix("jdk")
    case x                             => s"jdk@liberica-$x"
  }

  def index(jdkVersion: Seq[Int], url: String): Index = {
    val indexUrl = s"$indexArchiveType+$url"
    Index(indexOs, indexArch, indexJdkName, jdkVersion.take(3).mkString("."), indexUrl)
  }
}

def libericaIndex(): Index = {

  val oses = Seq("darwin", "linux", "windows", "linux-musl") // Add "solaris"
  val cpus = Seq("x86", "amd64", "arm", "arm64", "ppc64")
  val bundleTypes = Seq("jdk", "jre", "jdk-full", "jdk-lite", "jre-full")
  val allParams = for {
    os <- oses
    cpu <- cpus
    ext = if (os == "windows" || os == "darwin") "zip" else "tgz"
    bundleType <- bundleTypes
  } yield LibericaParams(os, cpu, ext, bundleType = bundleType)

  allParams
    .flatMap { params =>
      System.err.println(s"Getting ${params.url}")
      val resp = quickRequest.get(params.url).send()
      val json =
        try ujson.read(resp.body)
        catch {
          case NonFatal(e) =>
            System.err.println(s"Error parsing '${resp.body}'")
            throw e
        }

      val count = json.arr.length
      System.err.println(s"Found $count elements")

      json.arr
        .toArray
        .map(_.obj)
        .map { a =>
          val featureVersion = a("featureVersion").num.toInt
          val patchVersion = a("patchVersion").num.toInt
          val updateVersion = a("updateVersion").num.toInt
          val buildVersion = a("buildVersion").num.toInt
          (featureVersion, patchVersion, updateVersion, buildVersion) -> a
        }
        .sortBy(_._1)
        .iterator
        .map {
          case ((featureVersion, patchVersion, updateVersion, buildVersion), obj) =>
            val url = obj("downloadUrl").str
            val jdkVersion = List(featureVersion, patchVersion, updateVersion, buildVersion)
            params.index(jdkVersion, url)
        }
    }
    .foldLeft(Index.empty)(_ + _)
}

private lazy val ghToken = Option(System.getenv("GH_TOKEN")).getOrElse {
  System.err.println("Warning: GH_TOKEN not set, it's likely we'll get rate-limited by the GitHub API")
  ""
}

def fullGraalvmIndex(): Index = {
  val graalvmIndex0 = graalvmIndex(ghToken, "8")
  val graalvmJdk11Index0 = graalvmIndex(ghToken, "11")
  graalvmIndex0 + graalvmJdk11Index0
}

def fullAdoptIndex(): Index = {
  val adoptIndices = (8 to 16).map { num =>
    val versionPrefix = if (num == 8) "1." else ""
    adoptIndex(ghToken, num, versionPrefix)
  }
  adoptIndices.foldLeft(Index.empty)(_ + _)
}

@main
def printGraalvmIndex(): Unit = {
  val index = fullGraalvmIndex()
  println(index.json)
}

@main
def printAdoptIndex(): Unit = {
  val adopt8Index = adoptIndex(ghToken, 8, "1.")
  val adopt11Index = adoptIndex(ghToken, 11)
  val adoptIndex0 = adopt8Index + adopt11Index
  println(adoptIndex0.json)
}

@main
def writeIndex(output: String = "index.json"): Unit = {

  val graalvmIndex0 = fullGraalvmIndex()
  val adoptIndex0 = fullAdoptIndex()
  val zuluIndex0 = zuluIndex()
  val libericaIndex0 = libericaIndex()

  val json = (graalvmIndex0 + adoptIndex0 + zuluIndex0 + libericaIndex0).json
  val dest = Paths.get(output)
  Files.write(dest, json.getBytes("UTF-8"))
  System.err.println(s"Wrote $dest")
}

@main
def dummy(): Unit = {}
