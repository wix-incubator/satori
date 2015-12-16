package com.wix.satori.util

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
