package com.wix.satori.analysis

import com.wix.satori.model.Commit
import com.wix.satori.util.{IOHelpers, Logging}

trait RepositoryAnalyzer extends Logging with IOHelpers {
  self: Singleton =>

  type Configuration
  def emptyConfiguration: Configuration
  def configure(parser: scopt.OptionParser[Configuration]): Unit
  def analyze(config: Configuration): Iterator[Commit]
}
