import java.io.File

import org.eclipse.jgit.lib.Constants
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import scala.collection.JavaConversions._

object RepoAnalysis extends App {
  case class AnalysisConfiguration(repo: File = new File("."))

  private val parser = new scopt.OptionParser[AnalysisConfiguration]("RepoAnalysis") {
    opt[File]('f', "repo")
      .valueName("<path>")
      .action { case (f, config) => config.copy(repo = f) }
      .text("The path to the repository")
  }

  parser.parse(args, AnalysisConfiguration()).foreach { config => RepoAnalysis(config).run() }
}

import RepoAnalysis._
case class RepoAnalysis(config: AnalysisConfiguration) {

  private def using[T <: AutoCloseable, R](gen: =>T)(thunk: T => R): R = {
    val resource = gen
    try { thunk(resource) }
    finally { resource.close() }
  }

  def run(): Unit = {
    val repo = new FileRepositoryBuilder().setGitDir(config.repo).readEnvironment().findGitDir().build()
    val head = repo.resolve(Constants.HEAD)

    using(new RevWalk(repo)) { walk =>
      val root = walk.parseCommit(head)
      walk.markStart(root)
      walk foreach { commit =>
        val at = java.time.Instant.ofEpochSecond(commit.getCommitTime)
        println(s"Commit ${commit.getName} at $at: ${commit.getShortMessage} (${commit.getCommitterIdent.toExternalString})")
      }
    }
  }
}