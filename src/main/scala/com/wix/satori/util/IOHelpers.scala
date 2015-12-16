package com.wix.satori.util

/**
  * Created by tomerga on 12/16/15.
  */
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
