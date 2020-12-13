import scala.io.Source
val pat = "([A-Z])(\\d+)".r

val instructions = 
Source.fromFile("input.txt").getLines.map(
    _ match {
      case pat(x, y) => instruction(x.head, y.toInt)
      case _ => instruction('F', 0)
}).toArray

/* question 1 */

case class instruction(direction: Char , mag: Int)
case class state(x: Int, y: Int, deg: Int)

def updateState(s: state, inst: Array[instruction]): state = {
    inst.size match {
        case 0 => s
        case _ => updateState(updateDistance(s, inst.head), inst.tail)
    }
}

def updateDistance(s: state , nxt: instruction): state = {
    val actualDirection =
      nxt.direction match {
          case('F') => degToDirection(s.deg)
          case _ => nxt.direction
      }
    val n = nxt.mag
    val newCoords = actualDirection match {
        case('N') => (s.x, s.y+n)
        case('S') => (s.x, s.y-n)
        case('E') => (s.x+n, s.y)
        case('W') => (s.x-n, s.y)
        case _ => (s.x, s.y)
    }
    val newDeg = nxt.direction match {
        case('R') => s.deg + n
        case('L') => s.deg - n
        case _ => s.deg
    }

    state(newCoords._1, newCoords._2, newDeg)
}

val degToDirection = (x: Int) => {
    val adjusted = if (x >= 0) x % 360 else 360 - ((x % 360) * -1)
    adjusted match {
    case 0|360 => 'E'
    case 90 => 'S'
    case 180 => 'W'
    case 270 => 'N'
    }
}


/* question 2 */


case class stateWp(x: Int, y: Int, w_x: Int, w_y: Int)

def updateState2(s: stateWp, inst: Array[instruction]): stateWp = {
    inst.size match {
        case 0 => s
        case _ => updateState2(updateDistance2(s, inst.head), inst.tail)
    }
}

def updateDistance2(s: stateWp , nxt: instruction): stateWp = {
    val n = nxt.mag
    val newWp = nxt.direction match {
        case('N') => (s.w_x, s.w_y+n)
        case('S') => (s.w_x, s.w_y-n)
        case('E') => (s.w_x+n, s.w_y)
        case('W') => (s.w_x-n, s.w_y)
        case('L') => rotateLeft(n / 90)(s.w_x, s.w_y)
        case('R') => rotateRight(n / 90)(s.w_x, s.w_y)
        case _ => (s.w_x, s.w_y)
    }
    
    val newCoords= 
      nxt.direction match {
          case('F') => (s.x + s.w_x * n, s.y + s.w_y*n)
          case _ => (s.x, s.y)
      }
    stateWp(newCoords._1, newCoords._2, newWp._1, newWp._2)
}

val rotateRight = (n: Int) => {
    val rRight= (x:(Int,Int)) => (x._2, -x._1)
    (1 to n).map(_ => rRight).reduceLeft(_ andThen _)
}

val rotateLeft = (n: Int) => {
    val rLeft= (x:(Int,Int)) => (-x._2, x._1)
    (1 to n).map(_ => rLeft).reduceLeft(_ andThen _)
}

val q1 = updateState(state(0,0,0), instructions)
val q2 = updateState2(stateWp(0,0, 10, 1), instructions)

println(q1.x.abs + q1.y.abs) 
println(q2.x.abs + q2.y.abs) 
