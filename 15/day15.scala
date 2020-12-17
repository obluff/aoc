import scala.annotation.tailrec

@tailrec
def playGame(prev: Map[Int, Int], lastNum: Int, idx: Int, breakPoint: Int): Int = {
    if(idx % 10000000 == 0) {
        println(idx)
    }
    if(idx == breakPoint-1){
        return lastNum 
    }
    val numberSaid = 
      prev.contains(lastNum) match {
        case true => idx - prev(lastNum)
        case false => 0
      }
    
    return playGame(prev + (lastNum -> idx), numberSaid, idx + 1, breakPoint) 
}

def playFromStartingNumbers(numbers: List[Int], numIterations: Int): Int = {
    val mapped = numbers.zipWithIndex.map(x => (x._1 -> x._2)).toMap
    playGame(mapped, numbers.last, numbers.size - 1, numIterations)
}

val q1 = playFromStartingNumbers(List(2,0,1,7,4,14,18), 2020)
val q2 = playFromStartingNumbers(List(2,0,1,7,4,14,18), 30000000)

println(q1)
