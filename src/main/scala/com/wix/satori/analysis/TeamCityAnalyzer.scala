package com.wix.satori.analysis

import java.util.concurrent.Executors

import akka.actor.Actor.Receive
import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import akka.io.IO
import akka.util.Timeout
import com.wix.satori.util.{IOHelpers, Logging}
import spray.can.Http
import spray.http.Uri
import spray.json.DefaultJsonProtocol

import scala.concurrent.{ExecutionContext, Future, Await}
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

  implicit val system = ActorSystem()
  try parser.parse(args, Configuration()) foreach { config =>

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
      ))

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
    import spray.httpx.TransformerAux._

    val vcsRootsEndpoint = Uri("vcs-roots") resolvedAgainst baseEndpoint
    def vcsRoots = (baseRequest ~> sendReceive ~> unmarshal[VcsRootList]) apply Get(vcsRootsEndpoint)


    import akka.util.Timeout
    def throttle(concurrency: Int, via: ActorRef = IO(Http))(implicit futureTimeout: Timeout = 60.seconds): ActorRef =
      system.actorOf(Props(new Actor {
        import akka.pattern.ask
        import scala.collection.mutable

        private object Dec
        var remaining = concurrency
        var queue = mutable.Queue.empty[(Any, ActorRef)]

        def process(msg: Any, from: ActorRef) = {
          remaining -= 1
          via.ask(msg)(futureTimeout, from) onComplete { _ => self ! Dec }
        }

        def receive: Receive = {
          case Dec if remaining < concurrency =>
            remaining += 1
            while (queue.nonEmpty && remaining > 0) {
              val (msg, from) = queue.dequeue()
              debug(s"Processing enqueued message $msg (from $sender)")
              process(msg, from)
            }

          case msg if remaining > 0 =>
            process(msg, sender())

          case msg if remaining == 0 =>
            queue enqueue (msg -> sender())
        }
      }))

    def gitRoots = {
      implicit val timeout = akka.util.Timeout(60.seconds)

      val throttler = throttle(20)

      def getRoot(header: VcsRootHeader): Future[VcsRoot] = {
        info(s"Resolving VCS root ${header.name} at ${header.href}...")
        (baseRequest ~> sendReceive(throttler) ~> unmarshal[VcsRoot])
          .apply(Get(Uri(header.href) resolvedAgainst baseEndpoint))
      }

      debug("Loading VCS roots...")
      val resolvedRoots = vcsRoots flatMap { roots => Future.sequence(roots.`vcs-root` map getRoot) }
      val gitRoots = resolvedRoots map { roots =>
        debug("Filtering git-specific roots")
        roots.filter(_.vcsName == VcsRoot.gitVcsName)
      }
      gitRoots
    }

    Await.result(gitRoots, 5.minutes) foreach println
  } finally {
    system.terminate()
    Await.result(system.whenTerminated, 5.seconds)
  }
}

