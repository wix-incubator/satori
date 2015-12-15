package com.wix.satori

import java.io.{File, PrintStream}

import com.wix.satori.analysis.{GitRepositoryAnalyzer, Logging}
import com.wix.satori.output.CSVOutput
import scopt.OptionParser

object Driver extends App with Logging {
  val analyzer = GitRepositoryAnalyzer
  val defaultFormatter = CSVOutput
  val formatters = Seq(CSVOutput).map { f => f.shortName -> f }.toMap

  var target: Option[File] = None
  var formatter = defaultFormatter

  // Decorate parser
  val parser = new OptionParser[analyzer.Configuration]("satori") {
    opt[File]('o', "output")
      .valueName("<file>")
      .action { case (o, c) => target = Some(o); c }
      .text(s"Output file (defaults to standard output)")

    opt[String]("format")
      .valueName("<format>")
      .action {
        case ("csv", c) => formatter = CSVOutput; c
        case (f, _) => throw new IllegalArgumentException(s"Invalid formatter '$f' specified")
      }
      .text(s"Output format; can be one of ${formatters.keys.mkString("[", ", ", "]")}, defaults to ${defaultFormatter.shortName}")
  }
  analyzer.configure(parser)
  val config = parser.parse(args, analyzer.emptyConfiguration).get  // Already terminated on error

  def stream =
    target.map { f =>
      info(s"Redirecting output to ${f.getAbsolutePath}")
      new PrintStream(f)
    }.getOrElse(System.out)

  formatter.withOutput(stream) { output =>
    analyzer.analyze(config) foreach output
    info("Done")
  }
}
