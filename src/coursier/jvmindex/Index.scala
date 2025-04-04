package coursier.jvmindex

import Index.{Arch, Os}

final case class Index(map: Map[Os, Map[Arch, Map[String, Map[String, String]]]]) {

  def mapJdkName(f: String => String): Index =
    Index(
      map.map {
        case (os, map0) =>
          os -> map0.map {
            case (arch, map1) =>
              arch -> map1.map {
                case (jdkName, map2) =>
                  f(jdkName) -> map2
              }
          }
      }
    )

  def +(other: Index): Index =
    Index(Index.merge4(map, other.map))

  def json: String =
    Index.json4(map).render(indent = 2)

  def osArchIndices: Map[(Os, Arch), OsArchIndex] =
    map
      .toSeq
      .flatMap {
        case (os, osMap) =>
          osMap.toSeq.map {
            case (arch, osArchMap) =>
              val cleanedUpOs =
                if (os == Os("alpine-linux")) Os("linux-musl")
                else os
              val cleanedUp = osArchMap.map {
                case (jdkName, map) =>
                  jdkName.stripPrefix("jdk@") -> map.map {
                    case (version, url) =>
                      (version.stripPrefix("1."), url)
                  }
              }
              ((cleanedUpOs, arch), OsArchIndex(cleanedUp))
          }
      }
      .groupBy(_._1)
      .map {
        case (k, Seq())       => sys.error("Cannot happen after a groupMap")
        case (k, Seq((_, v))) => k -> v
        case (k, Seq(h, t @ _*)) =>
          k -> OsArchIndex(t.map(_._2.map).foldLeft(h._2.map)(Index.merge2))
      }

  def isEmpty: Boolean =
    map.isEmpty ||
    map.forall(_._2.isEmpty) ||
    map.forall(_._2.forall(_._2.isEmpty)) ||
    map.forall(_._2.forall(_._2.forall(_._2.isEmpty)))
}

object Index {

  opaque type Os = String
  object Os:
    def apply(value: String): Os        = value
    def unapply(os: Os): Option[String] = Some(os)

    given Ordering[Os] with
      def compare(a: Os, b: Os) = a.compareTo(b)

  opaque type Arch = String
  object Arch:
    def apply(value: String): Arch          = value
    def unapply(arch: Arch): Option[String] = Some(arch)

    given Ordering[Arch] with
      def compare(a: Arch, b: Arch) = a.compareTo(b)

  def empty: Index =
    Index(Map.empty)
  def apply(
    os: Os,
    architecture: Arch,
    jdkName: String,
    jdkVersion: String,
    url: String
  ): Index =
    Index(Map(os -> Map(architecture -> Map(jdkName -> Map(jdkVersion -> url)))))

  private def merge4(
    a: Map[Os, Map[Arch, Map[String, Map[String, String]]]],
    b: Map[Os, Map[Arch, Map[String, Map[String, String]]]]
  ): Map[Os, Map[Arch, Map[String, Map[String, String]]]] =
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

  private def json4(
    map: Map[Os, Map[Arch, Map[String, Map[String, String]]]]
  ) = {
    val l = map
      .toVector
      .sortBy(_._1)
      .map {
        case (os, m) =>
          os.toString() -> json3(m)
      }
    if (l.isEmpty)
      ujson.Obj()
    else
      ujson.Obj(l.head, l.tail*)
  }

  private def json3(
    map: Map[Arch, Map[String, Map[String, String]]]
  ) = {
    val l = map
      .toVector
      .sortBy(_._1)
      .map {
        case (k, m) =>
          k -> json2(m)
      }
    if (l.isEmpty)
      ujson.Obj()
    else
      ujson.Obj(l.head, l.tail*)
  }

  def json2(
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
      ujson.Obj()
    else
      ujson.Obj(l.head, l.tail*)
  }

  private def json1(
    map: Map[String, String]
  ) = {
    val list = map
      .toVector
      .map {
        case (k, v) =>
          coursier.version.Version(k) -> v
      }
      .sortBy(_._1)
    val requiredVersionLengthOpt = {
      val versionLengthMap =
        list.groupMap(_._1.items.length)(_ => ()).view.mapValues(_.length).toMap
      if (versionLengthMap.size > 1) Some(versionLengthMap.maxBy(_._2)._1.min(4))
      else None
    }
    val list0 = list.map {
      case (k, v) =>
        val version = requiredVersionLengthOpt match {
          case Some(requiredVersionLength) if k.items.length < requiredVersionLength =>
            k.repr + (".0" * (requiredVersionLength - k.items.length))
          case _ => k.repr
        }
        version -> ujson.Str(v)
    }
    if (list0.isEmpty)
      ujson.Obj()
    else
      ujson.Obj(list0.head, list0.tail*)
  }

}
