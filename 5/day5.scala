import scala.io.Source

// didnt realize it was a binary problem tbh 
// that's not my thang

val data = 
  Source.fromFile("input.txt")
        .getLines
        .toList

val preprocess = (x: String) =>
   x.replaceAll("F|L", "1")
    .replaceAll("B|R", "0")

val rowInstructs = (x: String) => x.slice(0, 7)
val colInstructs = (x: String) => x.slice(7, 11)

def getSeat(x: String): (Int, Int) = {
   val cleaned = preprocess(x)
   val row = bsp(rowInstructs(cleaned), 0, 128)
   val column = bsp(colInstructs(cleaned), 0, 8)
   (row, column) 
}

def bsp(instruct: String, lb: Int, ub: Int): Int = {
   if(lb == ub) return lb
   if(instruct == "") return lb

   val midPoint = (ub + lb) / 2
   instruct.head match {
       case '1' => bsp(instruct.tail, lb, midPoint)
       case '0' => bsp(instruct.tail, midPoint, ub)
   }
}
val calculateId = (x: (Int, Int)) => x._1 * 8 + x._2
val seats = data.map(getSeat)
val q1 = seats.map(calculateId).max


val min_row = seats.map(_._1).min * 8
val max_row = seats.map(_._1).max * 8
val w = seats.map(calculateId).sorted.filter(x => x > min_row && x < max_row)
val q2 = (w.min to w.max).sum - w.sum

println(q1)
println(q2)
