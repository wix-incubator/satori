package com.wix.satori.analysis

import java.time.Instant

import com.wix.satori.analysis.RepositoryAnalyzer.Commit
import org.slf4j.{Logger, LoggerFactory}

/**
  * Created by tomerga on 12/15/15.
  */
trait Logging {
  self =>

  private val log: Logger = LoggerFactory.getLogger(self.getClass)
  protected def debug(s: =>String) = if (log.isDebugEnabled) log.debug(s)
  protected def info(s: =>String) = if (log.isInfoEnabled) log.info(s)
  protected def warn(s: =>String) = if (log.isWarnEnabled) log.warn(s)
  protected def error(s: =>String) = if (log.isErrorEnabled) log.error(s)
}

trait IOHelpers {
  self: Logging =>

  def using[T <: AutoCloseable, R](gen: =>T)(thunk: T => R): R = {
    val resource = gen
    try { thunk(resource) }
    finally { resource.close() }
  }

  def die(s: =>String) = {
    error(s)
    System.exit(1)
    throw new Error()   // Fake exception throw since we can't directly return Nothing. WTF, Scala?
  }
}

trait RepositoryAnalyzer extends Logging with IOHelpers {
  self: Singleton =>

  type Configuration
  def emptyConfiguration: Configuration
  def configure(parser: scopt.OptionParser[Configuration]): Unit
  def analyze(config: Configuration): Iterator[Commit]
}

object RepositoryAnalyzer {
  val unknownLanguageMarker = "<unknown>"

  case class ChangeSetStatistics(deletedLOC: Int, addedLOC: Int) {
    def update(deletedLOC: Int, addedLOC: Int): ChangeSetStatistics =
      ChangeSetStatistics(this.deletedLOC + deletedLOC, this.addedLOC + addedLOC)
  }
  object ChangeSetStatistics { val empty = ChangeSetStatistics(0, 0) }

  case class ClassifiedStatistics(prod: ChangeSetStatistics, test: ChangeSetStatistics) {
    def update(deletedLOC: Int, addedLOC: Int, updateProd: Boolean): ClassifiedStatistics =
      ClassifiedStatistics(
        prod = if ( updateProd) prod.update(deletedLOC, addedLOC) else prod,
        test = if (!updateProd) test.update(deletedLOC, addedLOC) else test
      )
  }
  object ClassifiedStatistics { val empty = ClassifiedStatistics(ChangeSetStatistics.empty, ChangeSetStatistics.empty) }

  case class CommitStatistics(aggregate: ClassifiedStatistics, byLanguage: Map[String, ClassifiedStatistics])
  object CommitStatistics { val empty = CommitStatistics(ClassifiedStatistics.empty, Map.empty) }

  case class Commit(author: String, timestamp: Instant, hash: String, stats: CommitStatistics)
}
