val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", " Switzerland" -> " Bern")

capitalOfCountry("US")

//capitalOfCountry("Poland") // <- java.util.NoSuchElementException
capitalOfCountry.get("Poland ")
capitalOfCountry.get("US")

def showCapitol(country: String): String = capitalOfCountry.get(country) match {
  case Some(capitol) => capitol
  case None => "missing data"
}

showCapitol("Poland ")
showCapitol("US")
