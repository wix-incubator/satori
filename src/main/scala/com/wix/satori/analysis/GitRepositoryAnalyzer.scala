package com.wix.satori.analysis

import java.io.File
import java.time.Instant

import org.eclipse.jgit.diff.DiffEntry.ChangeType
import org.eclipse.jgit.diff._
import org.eclipse.jgit.diff.DiffAlgorithm.SupportedAlgorithm
import org.eclipse.jgit.revwalk.RevWalk
import org.eclipse.jgit.storage.file.FileRepositoryBuilder
import org.eclipse.jgit.treewalk.TreeWalk
import org.eclipse.jgit.treewalk.filter.TreeFilter

/**
  * Created by tomerga on 12/15/15.
  */
object GitRepositoryAnalyzer extends RepositoryAnalyzer {
  val defaultFrom = "master"

  case class Configuration(repo: File = new File("."), from: String = defaultFrom)

  val emptyConfiguration = Configuration()

  def configurationParser = new scopt.OptionParser[Configuration]("RepoAnalysis") {
    opt[File]('r', "repo")
      .valueName("<path>")
      .action { case (r, config) => config.copy(repo = r) }
      .text("The path to the repository (defaults to current working directory)")

    opt[String]('f', "from")
      .valueName("<ref>")
      .action { case (f, config) => config.copy(from = f) }
      .text(s"A Git reference from which to start traversal (defaults to '$defaultFrom')")
  }

  import RepositoryAnalyzer._
  import scala.collection.JavaConversions._

  def analyze(config: GitRepositoryAnalyzer.Configuration): Iterator[Commit] = {
    info(s"Starting repository analysis on ${config.repo} from ref ${config.from}")
    val repo = new FileRepositoryBuilder().readEnvironment().findGitDir(config.repo).build()
    val from = Option(repo.resolve(config.from)) getOrElse die(s"Can't resolve reference ${config.from}!")

    using(new RevWalk(repo)) { revWalk =>
      val algorithm = DiffAlgorithm.getAlgorithm(SupportedAlgorithm.HISTOGRAM)
      val reader = repo.newObjectReader()
      val comparator = RawTextComparator.WS_IGNORE_ALL

      revWalk.markStart(revWalk.parseCommit(from))

      revWalk.sliding(2) map { window =>
        val current = window.head   // TODO clean this crap up
        val prev = window.tail.head

        debug(s"Processing diff for ${prev.getName}..${current.getName}")
        val tree = new TreeWalk(repo)
        tree.addTree(prev.getTree)
        tree.addTree(current.getTree)
        tree.setRecursive(true)
        tree.setFilter(TreeFilter.ANY_DIFF)
        val entries = DiffEntry.scan(tree)

        import scala.collection.mutable
        // TODO lenses etc.
        var aggregate = ClassifiedStatistics.empty
        val byLanguage = mutable.Map.empty[String, ClassifiedStatistics]

        entries foreach { entry =>
          debug(s"Processing entry ${entry.getOldPath} (type=${entry.getChangeType}, target=${entry.getNewPath})")

          val isProd = entry.getOldPath.contains("src/main/")
          val language = entry.getOldPath.split('.').lastOption getOrElse unknownLanguageMarker
          def updateStats(deletedLOC: Int, addedLOC: Int, prod: Boolean = isProd): Unit = {
            aggregate = aggregate.update(deletedLOC, addedLOC, prod)
            byLanguage +=
              (language -> byLanguage.getOrElse(language, ClassifiedStatistics.empty).update(deletedLOC, addedLOC, prod))
          }

          entry.getChangeType match {
            case ChangeType.ADD | ChangeType.COPY =>
              val newLoader = reader.open(entry.getNewId.toObjectId)
              val lines = new RawText(newLoader.getBytes).size()
              updateStats(0, lines)

            case ChangeType.DELETE =>
              val newLoader = reader.open(entry.getOldId.toObjectId)
              val lines = new RawText(newLoader.getBytes).size()
              updateStats(lines, 0)

            case ChangeType.RENAME =>
            // TODO files may move from prod to test and vice versa, and may change languages as well

            case ChangeType.MODIFY =>
              val oldLoader = reader.open(entry.getOldId.toObjectId)
              val oldRaw = new RawText(oldLoader.getBytes)
              val newLoader = reader.open(entry.getNewId.toObjectId)
              val newRaw = new RawText(newLoader.getBytes)
              val diff = algorithm.diff(comparator, oldRaw, newRaw)
              diff foreach { edit =>
                debug(s"Processing edit hunk: [${edit.getBeginA}..${edit.getEndA}] -> [${edit.getBeginB}..${edit.getEndB}]")
                edit.getType match {
                  case Edit.Type.DELETE =>
                    updateStats(edit.getLengthA, 0)

                  case Edit.Type.EMPTY =>
                  // NOOP

                  case Edit.Type.INSERT =>
                    updateStats(0, edit.getLengthB)

                  case Edit.Type.REPLACE =>
                    updateStats(edit.getLengthA, edit.getLengthB)
                }
              }
          }
        }

        Commit(
          author    = current.getAuthorIdent.getEmailAddress,
          timestamp = Instant.ofEpochSecond(current.getCommitTime),
          hash      = current.getName,
          stats     = CommitStatistics(aggregate, byLanguage.toMap)
        )
      }
    }
  }
}
