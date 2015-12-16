package com.wix.satori

import java.time.Instant

/**
  * Created by tomerga on 12/16/15.
  */
package object model {
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
