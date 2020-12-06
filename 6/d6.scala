import scala.io.Source

val input = 
  Source.fromFile("input.txt")
       .mkString
       .split("\n\n")

val q1 = input.map(_.replace("\n", "").toSet.size).sum
val q2 = input.map(_.split("\n").map(_.toSet).reduceLeft(_&_).size).sum

println(q1)
println(q2)
