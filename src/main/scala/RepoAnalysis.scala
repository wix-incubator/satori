import java.io.File

import org.eclipse.jgit.revwalk.{RevCommit, RevWalk}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConversions._

object RepoAnalysis extends App {
  val defaultFrom = "master"

  case class AnalysisConfiguration(repo: File = new File("."), from: String = defaultFrom)

  private val parser = new scopt.OptionParser[AnalysisConfiguration]("RepoAnalysis") {
    opt[File]('r', "repo")
      .valueName("<path>")
      .action { case (r, config) => config.copy(repo = r) }
      .text("The path to the repository (defaults to current working directory)")

    opt[String]('f', "from")
      .valueName("<ref>")
      .action { case (f, config) => config.copy(from = f) }
      .text(s"A Git reference from which to start traversal (defaults to '$defaultFrom')")
  }

  parser.parse(args, AnalysisConfiguration()).foreach { config => RepoAnalysis(config).run() }
}

import RepoAnalysis._
case class RepoAnalysis(config: AnalysisConfiguration) {

  val log: Logger = LoggerFactory.getLogger(this.getClass)
  import log.{info, error}

  private def using[T <: AutoCloseable, R](gen: =>T)(thunk: T => R): R = {
    val resource = gen
    try { thunk(resource) }
    finally { resource.close() }
  }

  def die(s: =>String) = {
    error(s)
    System.exit(1)
    throw new Error()   // Fake exception throw since we can't directly return Nothing. WTF, Scala?
  }

  private def processCommit(commit: RevCommit): Unit = {
    val at = java.time.Instant.ofEpochSecond(commit.getCommitTime)
    println(s"Commit ${commit.getName} at $at: ${commit.getShortMessage} (${commit.getCommitterIdent.toExternalString})")
  }

  def run(): Unit = {
    info(s"Starting repository analysis on ${config.repo} from ref ${config.from}")
    val repo = new FileRepositoryBuilder().readEnvironment().findGitDir(config.repo).build()
    val head = Option(repo.resolve(config.from)) getOrElse die(s"Can't resolve reference ${config.from}!")

    using(new RevWalk(repo)) { walk =>
      val root = walk.parseCommit(head)
      walk.markStart(root)
      walk foreach processCommit
    }
  }
}