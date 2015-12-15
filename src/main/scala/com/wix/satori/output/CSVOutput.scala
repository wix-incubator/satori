package com.wix.satori.output

import java.io.PrintStream
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

import com.github.tototoshi.csv.CSVWriter
import com.wix.satori.analysis.RepositoryAnalyzer.Commit

/**
  * Created by tomerga on 12/15/15.
  */
object CSVOutput extends AnalysisOutput {
  private val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm.ss")

  def withOutput(ps: =>PrintStream)(thunk: (Commit => Unit) => Unit): Unit = {
    val writer = CSVWriter.open(ps)

    // TODO consider languages?
    writer.writeRow(Seq("hash", "author", "timestamp_utc", "prod_add_loc", "prod_del_loc", "test_add_loc", "test_del_loc"))
    def writeRow(commit: Commit) =
      writer.writeRow(Seq(
        commit.hash,
        commit.author,
        dateFormatter.format(commit.timestamp.atOffset(ZoneOffset.UTC)),
        commit.stats.aggregate.prod.addedLOC,
        commit.stats.aggregate.prod.deletedLOC,
        commit.stats.aggregate.test.addedLOC,
        commit.stats.aggregate.test.deletedLOC
      ))

    try thunk(writeRow)
    finally { writer.close() }
  }

  def shortName = "csv"
}
