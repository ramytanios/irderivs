package lib

abstract class Error(msg: String) extends RuntimeException(msg)

object Error:

  class Generic(msg: String) extends Error(msg)

  def apply(msg: String): Error = Error.Generic(msg)
