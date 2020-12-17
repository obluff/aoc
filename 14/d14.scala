import java.lang.Long.parseLong
import scala.io.Source

case class instr(addr: Int, bytes: String)

val toLong = (x: String) => parseLong(x, 2)
val pat = "mem\\[(\\d+)\\] = (\\d+)".r
val padToBinary = (x: String) => x.toInt.toBinaryString.reverse.padTo(36, "0").reverse.mkString
val toInstruction = (x: String) => {
    x match {
    case pat(x, y) => {
      instr(x.toInt, 
            padToBinary(y))
      }
    }
}

def applyMask(mask: String, num: String): Long= {
    val fixedBin = 
     (mask zip num).map({
        case(mask: Char, bit: Char) => {
            mask match {
                case 'X' => bit
                case _ => mask
            }
         }
     }).mkString
    toLong(fixedBin)
}

def runInstructions(mask: String, records: List[instr], mem: Map[Int, Long] = Map[Int, Long]()): Map[Int, Long] = {
    records.size match {
        case 0 => mem
        case _ => {
            val curr = records.head
            runInstructions(mask, 
                            records.tail,
                            mem + (curr.addr -> applyMask(mask, curr.bytes)))
    }
  }
}

case class instr2(addr: String, bytes: Long)

val toInstruction2 = (x: String) => {
    x match {
    case pat(x, y) => {
         instr2(padToBinary(x),
                y.toLong)   
      }
  }
}

def applyMask2(mask: String, addr: String): String = {
     (mask zip addr).map({
        case(mask: Char, bit: Char) => {
            mask match {
                case 'X' => 'X'
                case '1' => mask
                case _ => bit
            }
         
     }
     }).mkString
}

def runInstructions2(mask: String, records: List[instr2], mem: Map[Long, Long] = Map[Long, Long]()): Map[Long, Long] = {
    records.size match {
        case 0 => mem
        case _ => {
            val curr = records.head
            val maskUpdate = applyMask2(mask, curr.addr)
            val memUpdates = memoryInvasion(maskUpdate).map(x => (toLong(x) -> curr.bytes)).toMap 
            runInstructions2(mask, 
                            records.tail,
                            mem ++ memUpdates)
        }
    }
}

def memoryInvasion(x: String, results: Array[String]=Array[String]("")): Array[String] = {
    if(x == ""){
        return results
    }
    val end =  
    x.head match {
        case 'X' => results.map(_ ++ "0") ++ results.map(_ ++ "1")
        case _ => results.map(_ ++ x.head.toString)
    }
    memoryInvasion(x.tail, end)
}


val inp =  
  Source.fromFile("input.txt")
        .mkString

val zipped = 
  inp.split("\n").filter(_.contains("mask")) zip inp.split("\n?mask.*\n").tail

val q1 = 
  zipped.map{case(mask, item) => {
            (mask.split(" = ")(1),
             item.split("\n").map(toInstruction))}}
        .map{case(mask, instructions) => {
            runInstructions(mask, instructions.toList, Map[Int, Long]())}}
        .reduce((x, y) => x ++ y)
        .map(_._2).sum

val q2 = 
  zipped.map{case(mask, item) => {
            (mask.split(" = ")(1),
             item.split("\n").map(toInstruction2))}}
        .map{case(mask, instructions) => {
            runInstructions2(mask, instructions.toList)}}
        .reduce((x, y) => x ++ y)
        .map(_._2).sum


println(q1)
println(q2)
