object Main {

  enum School {

    case Graduate
    case Undergraduate
    case PostGraduate

  }

  inline def classify(value: Any) = {
    inline value match {
      case int: Int    => "Int"
      case str: String => "String"
    }

    def admission(school: School) = school match {
      case School.Graduate      => 2
      case School.Undergraduate => 1
      case School.PostGraduate  => 3
    }
  }

  @main def run = Main.classify(2)

  inline def hello: Unit =
    inline if (debugLogEnabled) {
      println("debug is enabled")
    } else {
      println("debug is disabled")
    }

  val debugLogEnabled = true

}
