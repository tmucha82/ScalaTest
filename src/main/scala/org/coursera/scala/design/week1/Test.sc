val f: String => String = {
  case "ping" => "pong"
}
f("ping")
//f("abc") //MatchError

val f2: PartialFunction[String, String] = {
  case "ping" => "pong"
}
f2("ping")
f2.isDefinedAt("abc")
f2.isDefinedAt("ping")