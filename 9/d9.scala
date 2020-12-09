import scala.io.Source

val input = 
Source.fromFile("input.txt")
      .getLines
      .map(_.toLong)
      .toList

val q1 = 
  input.sliding(26, 1)
   .map(x => (x.dropRight(1), x.last))
   .find{case(nums, target) => nums.combinations(2).find(_.sum == target).size == 0}
   .map(_._2)
   .getOrElse(0)
 
// laaazyy
val q2 = (2 to input.size).view.map(input.sliding(_, 1)).flatten.find(_.sum == q1).map(x => x.min + x.max).getOrElse(0)

println(q1)
println(q2)


