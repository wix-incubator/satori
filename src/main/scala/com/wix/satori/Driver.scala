package com.wix.satori

import com.wix.satori.analysis.{RepositoryAnalyzer, GitRepositoryAnalyzer}

object Driver extends App {
  def analyze(analyzer: RepositoryAnalyzer) = {
    val config = analyzer.configurationParser.parse(args, analyzer.emptyConfiguration).get  //TODO
    analyzer.analyze(config)
  }

  analyze(GitRepositoryAnalyzer) take 5 foreach println
}
