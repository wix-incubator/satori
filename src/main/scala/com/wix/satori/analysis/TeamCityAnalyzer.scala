package com.wix.satori.analysis

import akka.actor.ActorSystem
import com.wix.satori.util.{IOHelpers, Logging}
import spray.http.Uri
import spray.json.DefaultJsonProtocol

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._

/**
  * Created by tomerga on 12/16/15.
  */
object TeamCityAnalyzer extends App with Logging with IOHelpers {


  case class Configuration(url: Uri = Uri.Empty.withScheme("http"),
                           user: Option[String] = None,
                           password: Option[String] = None) {
    private[TeamCityAnalyzer] def userinfo = for (u <- user; p <- password) yield s"$user:$password"
  }

  val parser = new scopt.OptionParser[Configuration]("TeamCityAnalyzer") {
    opt[String]("url")
      .valueName("<url>")
      .text("The TeamCity server URL")
      .action { case (url, c) => c.copy(url = Uri(url)) }

    opt[String]('h', "host")
      .valueName("<host>")
        .text("The TeamCity server host name")
      .action { case (host, c) => c.copy(url = c.url withHost host) }

    opt[Int]('P', "port")
      .valueName("<port>")
      .text("The TeamCity server port")
      .action { case (port, c) => c.copy(url = c.url withPort port) }

    opt[String]('u', "user")
      .required()
      .valueName("<user>")
      .text("The user name for authenticating with TeamCity")
      .action { case (user, c) => c.copy(user = Some(user)) }

    opt[String]('p', "pass")
      .required()
      .valueName("<password>")
      .text("The password for authenticating with TeamCity")
      .action { case (password, c) => c.copy(password = Some(password)) }
  }

  val config = parser.parse(args, Configuration()).getOrElse(die("Cannot parse command line"))

  implicit val system = ActorSystem()
  sys.addShutdownHook { system.terminate(); Await.result(system.whenTerminated, 5.seconds) }

  import spray.client.pipelining._
  import spray.http._
  import system.dispatcher

  val baseEndpoint =
    Uri("/httpAuth/app/rest/").resolvedAgainst(config.url)
  val baseRequest =
    addHeader(HttpHeaders.Accept(MediaTypes.`application/json`)) ~>
    addCredentials(BasicHttpCredentials(
      username = config.user.getOrElse(die("Username not specified")),
      password = config.password.getOrElse(die("Password not specified"))
    )) ~>
    sendReceive

  case class VcsRootHeader(id: String, name: String, href: String)
  case class VcsRootList(count: Int, `vcs-root`: List[VcsRootHeader])
  case class ProjectHeader(id: String, name: String, parentProjectId: Option[String], href: String, webUrl: String)
  case class Property(name: String, value: Option[String])
  case class PropertySet(count: Int, property: List[Property])
  case class VcsRoot(id: String,
                     name: String,
                     vcsName: String,
                     status: String,
                    // lastChecked: OffsetDatetime,
                     project: ProjectHeader,
                     properties: PropertySet
                    )
  object VcsRoot {
    val gitVcsName = "jetbrains.git"
  }
/*
{
  "vcsName": "jetbrains.git",
  "lastChecked": "20151202T060547+0000",
  "properties": {
    "count": 9,
    "property": [
      {
        "name": "agentCleanFilesPolicy",
        "value": "ALL_UNTRACKED"
      },
      {
        "name": "agentCleanPolicy",
        "value": "ON_BRANCH_CHANGE"
      },
      {
        "name": "authMethod",
        "value": "PRIVATE_KEY_DEFAULT"
      },
      {
        "name": "branch",
        "value": "refs/heads/master"
      },
      {
        "name": "ignoreKnownHosts",
        "value": "true"
      },
      {
        "name": "submoduleCheckout",
        "value": "CHECKOUT"
      },
      {
        "name": "url",
        "value": "git@github.com:wix/wix-public.git"
      },
      {
        "name": "useAlternates",
        "value": "true"
      },
      {
        "name": "usernameStyle",
        "value": "USERID"
      }
    ]
  },
  "vcsRootInstances": {
    "href": "/httpAuth/app/rest/vcs-root-instances?locator=vcsRoot:(id:CI_Scripts_A)"
  }
 */

  trait TeamCityBaseProtocol {
    self: DefaultJsonProtocol =>

    implicit val propertyFormat = jsonFormat2(Property)
    implicit val propertySetFormat = jsonFormat2(PropertySet)
    implicit val projectHeaderFormat = jsonFormat5(ProjectHeader)
  }

  trait VcsRootProtocol {
    self: DefaultJsonProtocol with TeamCityBaseProtocol =>

    implicit val vcsRootHeaderFormat = jsonFormat3(VcsRootHeader)
    implicit val vcsRootListFormat = jsonFormat2(VcsRootList)
    implicit val vcsRootFormat = jsonFormat6(VcsRoot.apply)
  }

  object TeamCityProtocol extends DefaultJsonProtocol with TeamCityBaseProtocol with VcsRootProtocol
  import TeamCityProtocol._
  import spray.httpx.SprayJsonSupport._

  val vcsRootsEndpoint = Uri("vcs-roots") resolvedAgainst baseEndpoint
  def vcsRoots = (baseRequest ~> unmarshal[VcsRootList]) apply Get(vcsRootsEndpoint)

  def gitRoots = {
    def getRoot(header: VcsRootHeader): Future[VcsRoot] =
      (baseRequest ~> unmarshal[VcsRoot]) apply Get(Uri(header.href) resolvedAgainst baseEndpoint)

    val resolvedRoots = vcsRoots flatMap { roots => Future.sequence(roots.`vcs-root` map getRoot) }
    val gitRoots = resolvedRoots map { _.filter(_.vcsName == VcsRoot.gitVcsName) }
    gitRoots
  }
//    for {
//      rootList <- vcsRoots
//    }
//    yield for {
//      header <- rootList.`vcs-root`
//      root <-
//      if root.vcsName == VcsRoot.gitVcsName
//    }
//    yield root.id -> root.properties.properties.find(_.name == "url").getOrElse(die("Git VCS root with no URL"))

  Await.result(gitRoots, 2.minutes) foreach println
}

