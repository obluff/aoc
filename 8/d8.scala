import scala.io.Source

case class instruction(name: String, value: Int)

def q1Boyy(instructionList: List[instruction], idx: Int = 0, acc: Int = 0, visited: Set[Int]=Set[Int]()): Int = {
    val curr = instructionList(idx)
    if(visited.contains(idx)) {
        return acc
    }
    val newSet = visited | Set(idx)
    curr.name match {
        case("nop") => q1Boyy(instructionList, idx + 1, acc, newSet)
        case("acc") => q1Boyy(instructionList, idx + 1, acc + curr.value, newSet)
        case("jmp") => q1Boyy(instructionList, idx + curr.value, acc, newSet)
    }
}

def programRepeats(instructionList: List[instruction], idx: Int = 0, acc: Int = 0, visited: Set[Int]=Set[Int]()): Boolean = {
    if(idx == instructionList.size){
        return true
    }
    if(idx > instructionList.size){
        return false
    }
    val curr = instructionList(idx)
    if(visited.contains(idx)) {
        return false
    }
    val newSet = visited | Set(idx)
    curr.name match {
        case("nop") => programRepeats(instructionList, idx + 1, acc, newSet)
        case("acc") => programRepeats(instructionList, idx + 1, acc + curr.value, newSet)
        case("jmp") => programRepeats(instructionList, idx + curr.value, acc, newSet)
    }
}


def run(instructionList: List[instruction], idx: Int = 0, acc: Int = 0, visited: Set[Int]=Set[Int](), swapped: Boolean = false): Int = {
    if(idx == instructionList.size){
        return acc
    }
    val curr = instructionList(idx)
    if(visited.contains(idx)) {
        return acc
    }
    val newSet = visited | Set(idx)
    
    if(!swapped){
      return curr.name match {
            case("nop") => {
                if(programRepeats(instructionList, idx + curr.value , acc, newSet)) {
                    run(instructionList, idx + curr.value, acc, newSet, true)
                }
                else{
                    run(instructionList, idx + 1, acc, newSet, false)
                }
            }
            case("acc") => run(instructionList, idx + 1, acc + curr.value, newSet, swapped)
            case("jmp") =>  {
                if(programRepeats(instructionList, idx + 1, acc, newSet)) {
                    run(instructionList, idx+1, acc, newSet, true)
                }
                else{
                    run(instructionList, idx + curr.value, acc, newSet, false)
                }
            }
        }
    }
   curr.name match {
     case("nop") => run(instructionList, idx + 1, acc, newSet, true)
     case("acc") => run(instructionList, idx + 1, acc + curr.value, newSet, true)
     case("jmp") => run(instructionList, idx + curr.value, acc, newSet, true)
   }
}

val toNum = (x: String) => if(x.head == '+') 1 * x.tail.toInt else -1 * x.tail.toInt

val b = 
  Source.fromFile("input.txt")
        .getLines
        .toList
        .map(_.split(" "))
        .map(x => instruction(x(0), toNum(x(1))))


println(q1Boyy(b))

println(run(b))
