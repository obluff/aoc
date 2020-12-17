val lines = scala.io.Source.fromFile("input.txt").getLines().map(_.toLong).toList
val errorVal = lines.sliding(26).find { x => !x.init.combinations(2).map(_.sum).contains(x.last) }.get.last
val error2Val = (2 to lines.length).flatMap { i => lines.sliding(i).find(_.sum == errorVal).map(x => x.min + x.max) }.head
println(errorVal)
println(error2Val) 
