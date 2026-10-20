package coursier.jvmindex

import scala.util.control.NonFatal

final case class Asset(
  name: String,
  downloadUrl: String,
  digest: Option[String]
)

object Asset {

  private def checksumFileSuffixes = Seq(".sha256.txt", ".sha256")

  /** Index entry for an asset
    *
    * Its checksum is taken from the GitHub digest of the asset if it has one. Older assets don't
    * have one, so we point to the checksum file alongside them in that case, if there's one.
    *
    * @param assetsByName
    *   all the assets of the release of `asset`, by name
    */
  def indexEntry(
    asset: Asset,
    archiveType: String,
    assetsByName: Map[String, Asset]
  ): IndexEntry =
    IndexEntry(
      archiveType,
      asset.downloadUrl,
      checksums = asset.digest.map(Checksums.fromGitHubDigest).getOrElse(Map.empty),
      checksumFileUrl = checksumFileSuffixes
        .flatMap(suffix => assetsByName.get(asset.name + suffix))
        .headOption
        .map(_.downloadUrl)
    )

  def releaseAssets(
    ghOrg: String,
    ghProj: String,
    ghToken: String,
    tagName: String
  ): Iterator[Asset] = {

    def helper(before: Option[String]): Iterator[Asset] = {
      System.err.println(
        s"Getting assets of $ghOrg/$ghProj for release $tagName${before.fold("")(" before " + _)} …"
      )
      val resp = GitHub.queryRepo(ghOrg, ghProj, ghToken) {
        s"""|release(tagName: "$tagName") {
            |  releaseAssets(
            |    ${before.fold("")(cursor => s"before: \"$cursor\"")}
            |    last: 100
            |  ) {
            |    nodes { name downloadUrl digest }
            |    pageInfo { hasPreviousPage, startCursor }
            |  }
            |}""".stripMargin
      }
      val json = resp("release")("releaseAssets")

      val res =
        try json("nodes").arr.map { obj =>
            Asset(obj("name").str, obj("downloadUrl").str, obj("digest").strOpt)
          }
        catch {
          case NonFatal(e) =>
            System.err.println(json)
            throw e
        }

      val pageInfo = json("pageInfo")
      if (pageInfo("hasPreviousPage").bool)
        res.iterator ++ helper(Some(pageInfo("startCursor").str))
      else
        res.iterator
    }

    helper(None)
  }

}
