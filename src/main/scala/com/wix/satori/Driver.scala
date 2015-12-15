package com.wix.satori

import com.github.tototoshi.csv._
import com.wix.satori.analysis.{GitRepositoryAnalyzer, RepositoryAnalyzer}

object Driver extends App {
  def analyze(analyzer: RepositoryAnalyzer) = {
    val config = analyzer.configurationParser.parse(args, analyzer.emptyConfiguration).get  //TODO
    analyzer.analyze(config)
  }

  val w = CSVWriter.open(System.out)
  // Consider: languages?
  w.writeRow(Seq("hash", "author", "timestamp_utc", "prod_add_loc", "prod_del_loc", "test_add_loc", "test_del_loc"))
  try {
    analyze(GitRepositoryAnalyzer) take 5 foreach { commit =>
      w.writeRow(Seq(
        commit.hash,
        commit.author,
        commit.timestamp,
        commit.stats.aggregate.prod.addedLOC,
        commit.stats.aggregate.prod.deletedLOC,
        commit.stats.aggregate.test.addedLOC,
        commit.stats.aggregate.test.deletedLOC
      ))
    }
  } finally { w.close() }
}
