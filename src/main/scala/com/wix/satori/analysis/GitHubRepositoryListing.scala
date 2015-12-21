package com.wix.satori.analysis

import akka.actor.ActorSystem
import com.wix.satori.util.{IOHelpers, Logging}
import scopt.OptionParser
import spray.httpx.unmarshalling.FromResponseUnmarshaller
import spray.json.DefaultJsonProtocol
import scala.concurrent.Future
import spray.client.pipelining._
import spray.http._
import scala.concurrent.Await
import scala.concurrent.duration._

/**
  * Created by tomerga on 12/21/15.
  */
object GitHubRepositoryListing extends App with Logging with IOHelpers {

  case class Configuration(userAuthToken: String = null, orgs: Set[String] = Set.empty  )

  val parser = new OptionParser[Configuration]("GitHubRepositoryListing") {
    opt[String]('u', "user")
      .valueName("<token>")
      .required
      .text("GitHub user authentication token (see https://github.com/settings/tokens)")
      .action { case (t, c) => c.copy(userAuthToken = t) }

    opt[String]('o', "org")
      .unbounded
      .valueName("<organization>")
      .text("Limits repositories to the specified GitHub organization(s)")
      .action { case (o, c) => c.copy(orgs = c.orgs + o) }
  }

  implicit val system = ActorSystem()

  import system.dispatcher

  lazy val `application/vnd.github.v3+json`  =
    MediaTypes.register(MediaType.custom("application/vnd.github.v3+json"))


  case class Owner(login: String, id: Int, url: String, `type`: String)
  case class Repository(id: Int, owner: Owner, full_name: String, ssh_url: String)

  object GitHubProtocol extends DefaultJsonProtocol {
    implicit val ownerFormat = jsonFormat4(Owner)
    implicit val repositoryFormat = jsonFormat4(Repository)
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

    def orgFilter(r: Repository) =
      config.orgs.isEmpty ||
        (r.owner.`type` == "Organization" && config.orgs.contains(r.owner.login))

    Await.result(repos, 5.minutes) filter orgFilter foreach { r => println(r.ssh_url) }
  } finally {
    system.terminate()
    Await.ready(system.whenTerminated, 5.seconds)
  }
}
