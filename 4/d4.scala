import scala.io.Source

val input = 
  Source.fromFile("input.txt")
        .mkString
        .split("\n\n")
        .map(_.replace("\n", " ")
              .split(" ").map(_.split(":")))

val reqFields =
    Set("byr",
        "iyr",
        "eyr",
        "hgt",
        "hcl",
        "ecl",
        "pid")

val numRange = (lb: Int, ub: Int) => (x: String) => (x.toInt >= lb && x.toInt <= ub)

def evaluateFields(fieldName: String, entry: String): Boolean = {
    fieldName match {
        case "byr" => numRange(1920, 2002)(entry)
        case "iyr" => numRange(2010, 2020)(entry)
        case "eyr" => numRange(2020, 2030)(entry) 
        case "hgt" => evaluateHeight(entry)
        case "hcl" => entry.matches("\\#(\\d|[a-f]){6}") 
        case "ecl" => entry.matches("amb|blu|brn|gry|grn|hzl|oth")
        case "pid" => entry.matches("\\d{9}")
        case _ => false
    }
}

val evaluateHeight = (entry: String) => {
    val groups = "(\\d+)(cm|in)".r
    entry match {
       case groups(height, typ) => {
           typ match {
               case("cm") => numRange(150, 193)(height)
               case("in") => numRange(59, 76)(height)
               case _ => false
           }
       } 
       case _ => false
    }
}

val q1 = input.count(x => (reqFields diff (x.map(_(0)).toSet)).size == 0)
val q2 = input.count(passport => passport.count(entry => evaluateFields(entry(0), entry(1))) == 7)

println(q1)
println(q2)
