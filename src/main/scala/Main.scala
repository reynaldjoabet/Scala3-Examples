object Main {
  inline def classify(value: Any) = {
    inline value match {
      case int: Int    => "Int"
      case str: String => "String"
    }
  }
  @main def run = classify(2)

  inline def hello: Unit =
    inline if (debugLogEnabled) {
      println("debug is enabled")
    } else {
      println("debug is disabled")
    }
  val debugLogEnabled = true
}
