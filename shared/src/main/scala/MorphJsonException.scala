package edu.furman.classics.fumorph

  case class MorphJsonException(message: String = "", cause: Option[Throwable] = None) extends Exception(message) {
    cause.foreach(initCause)
  }
