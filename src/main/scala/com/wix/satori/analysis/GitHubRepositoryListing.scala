package com.wix.satori.analysis

import akka.actor.ActorSystem
import com.wix.satori.analysis.TeamCityAnalyzer.VcsRootList
import com.wix.satori.util.{IOHelpers, Logging}
import scopt.OptionParser
import spray.http._
import spray.httpx.unmarshalling.FromResponseUnmarshaller
import spray.json.DefaultJsonProtocol

import scala.concurrent.Future

/**
  * Created by tomerga on 12/21/15.
  */
object GitHubRepositoryListing extends App with Logging with IOHelpers {

  case class Configuration(userAuthToken: String = null, orgs: Set[String] = Set.empty  )

  val parser = new OptionParser[Configuration]("GitHubRepositoryListing") {
    opt[String]('u', "user")
      .valueName("<token>")
      .required()
      .text("GitHub user authentication token (see https://github.com/settings/tokens)")
      .action { case (t, c) => c.copy(userAuthToken = t) }

    opt[String]('o', "org")
      .valueName("<organization>")
      .text("Limits repositories to the specified GitHub organization(s)")
      .action { case (o, c) => c.copy(orgs = c.orgs + o) }
  }

  implicit val system = ActorSystem()

  import spray.client.pipelining._
  import spray.http._
  import system.dispatcher
  import scala.concurrent.Await
  import scala.concurrent.duration._

  lazy val `application/vnd.github.v3+json`  =
    MediaTypes.register(MediaType.custom("application/vnd.github.v3+json"))


  case class Repository(id: Int, full_name: String, ssh_url: String)

  object GitHubProtocol extends DefaultJsonProtocol {
    implicit val repositoryFormat = jsonFormat3(Repository)
  }

  try parser.parse(args, Configuration()) foreach { config =>
    val apiV3Header = addHeader(HttpHeaders.Accept(MediaRange(`application/vnd.github.v3+json`)))
    val apiBaseEndpoint = Uri("https://api.github.com")
    val authHeader =
      addHeader(HttpHeaders.Authorization(GenericHttpCredentials("token", config.userAuthToken)))
    val baseRequest = apiV3Header ~> authHeader ~> sendReceive

    val userRepos = Uri("/user/repos") resolvedAgainst apiBaseEndpoint
    import GitHubProtocol._
    import spray.httpx.SprayJsonSupport._

    def paginated[T](initialUri: Uri)(implicit u: FromResponseUnmarshaller[List[T]]) = {
      val reqPrototype = baseRequest ~> { resp: HttpResponse =>
        val nextUri =
          resp.header[HttpHeaders.Link].flatMap(_.values.find(_.params.contains(HttpHeaders.Link.next)).map(_.uri))

        (nextUri, unmarshal[List[T]].apply(resp))
      }

      def paginateOver(uri: Uri): Future[List[T]] = {
        debug(s"Paging over $uri")
        (reqPrototype apply Get(uri)) flatMap {
         case (Some(continuation), results) =>
           paginateOver(continuation) map { r => results ::: r }
         case (None, results) =>
           Future.successful(results)
       }
      }

      paginateOver(initialUri)
    }

    val repos = paginated[Repository](userRepos)
    val r = Await.result(repos, 5.minutes)
    r foreach println
  } finally {
    system.terminate()
    Await.ready(system.whenTerminated, 5.seconds)
  }
}
