package confuse.api

class ReadException(val message: String, cause: Throwable = null)
  extends Exception(message, cause)
