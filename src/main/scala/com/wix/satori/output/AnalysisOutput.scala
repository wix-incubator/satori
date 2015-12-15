package com.wix.satori.output

import java.io.PrintStream

import com.wix.satori.analysis.RepositoryAnalyzer.Commit

/**
  * Created by tomerga on 12/15/15.
  */
trait AnalysisOutput {
  self: Singleton =>

  def withOutput(ps: =>PrintStream)(thunk: (Commit => Unit) => Unit): Unit
  def shortName: String
}
