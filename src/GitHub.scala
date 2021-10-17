import sttp.client3.quick._

import scala.util.control.NonFatal

object GitHub {
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

    try ujson.read(resp.body)("data")("repository")
    catch { case NonFatal(e) => println(body); println(resp); throw e }
  }
}
