package com.wix.satori

import java.io.PrintStream
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter

import com.github.tototoshi.csv._
import com.wix.satori.analysis.GitRepositoryAnalyzer

object Driver extends App {
  import GitRepositoryAnalyzer._
  val config = configurationParser.parse(args, emptyConfiguration).get  //TODO

  val w = CSVWriter.open(config.output map { new PrintStream(_) } getOrElse System.out)
  try {
    // Consider: languages?
    w.writeRow(Seq("hash", "author", "timestamp_utc", "prod_add_loc", "prod_del_loc", "test_add_loc", "test_del_loc"))
    val dateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm.ss")
    analyze(config) foreach { commit =>
      w.writeRow(Seq(
        commit.hash,
        commit.author,
        dateFormatter.format(commit.timestamp.atOffset(ZoneOffset.UTC)),
        commit.stats.aggregate.prod.addedLOC,
        commit.stats.aggregate.prod.deletedLOC,
        commit.stats.aggregate.test.addedLOC,
        commit.stats.aggregate.test.deletedLOC
      ))
    }
  } finally {
    w.close()
  }
}
