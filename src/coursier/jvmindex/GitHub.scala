package coursier.jvmindex

import sttp.client3.quick._
import sttp.model.HeaderNames

import scala.annotation.tailrec
import scala.util.control.NonFatal

object GitHub {
  @tailrec
  def queryRepo(
    owner: String,
    name: String,
    ghToken: String
  )(
    q: String
  ): ujson.Value = {
    val body = ujson.Obj(
      "query" -> ujson.Str(s"""query { repository(owner: "$owner" name: "$name") { $q } }""")
    )

    val resp = quickRequest
      .headers {
        if (ghToken.isEmpty) Map[String, String]()
        else Map("Authorization" -> s"token $ghToken")
      }
      .body(body.render())
      .post(uri"https://api.github.com/graphql")
      .send(backend)

    val json = ujson.read(resp.body)

    if (!json.obj.contains("data"))
      pprint.err.log(resp)

    val retryDelayOpt = resp.header(HeaderNames.RetryAfter)
      .filter(_ => !json.obj.contains("data"))
      .flatMap(_.toIntOption)
    retryDelayOpt match {
      case Some(delaySeconds) =>
        System.err.println(s"GitHub rate limit exceeded, waiting $delaySeconds seconds")
        Thread.sleep(delaySeconds * 1000L)
        queryRepo(owner, name, ghToken)(q)
      case None =>
        if (
          !json.obj.contains("data") &&
          json.obj.get("message").exists(_.str.contains("API rate limit exceeded"))
        ) {
          System.err.println("GitHub rate limit exceeded, waiting one minute")
          Thread.sleep(60L * 1000L)
          queryRepo(owner, name, ghToken)(q)
        }
        else
          try json("data")("repository")
          catch { case NonFatal(e) => println(body); println(resp); throw e }
    }
  }
}
