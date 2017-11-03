package com.github.abeln.tamarin

/**
  * Error utilities
  */
object Err {

  def err(msg: String) = throw new RuntimeException(msg)

}
