import scala.io.Source

// q1 

val nextQ1 = (x: Array[Array[Char]]) => {
    padMatrix(x).sliding(3,1)
     .map(updateRow)
     .toArray
}

def padMatrix(x: Array[Array[Char]]): Array[Array[Char]] = {
    val topPadding = (1 to x(0).size + 2).map(_ => '.').toArray
    Array(topPadding) ++ x.map(Array('.') ++ _ ++ Array('.')) ++ Array(topPadding)
}

def updateRow(oneRow: Array[Array[Char]]): Array[Char] = {
    oneRow.transpose
          .sliding(3, 1)
          .map(_.flatten)
          .map(x => (x(4), x))
          .map{case(center, rest) => {
              center match {
              case 'L' => if(rest.count(_ == '#') == 0) '#' else 'L'
              case '#' => if(rest.count(_ == '#') >= 5) 'L' else '#'
              case _ => center
               }
            }
              }.toArray
          
}

/* Q2::: really wanted to avoid writing something 
   like this in the first one but HERE WE ARE
**/

val nextQ2 = (arr: Array[Array[Char]]) =>  {
    val coords = (0 to arr.size - 1).map(x => (0 to arr(0).size - 1).map((x, _)))
    coords.map(_.map(x => updateData(arr, x._1, x._2)).toArray).toArray
}

def getAdjacent(dx: Int, dy: Int)(arr: Array[Array[Char]], x: Int, y: Int): Char  = {
    if(x+dx < 0 || x+dx > arr.size - 1){
        return '.'
    }
    if(y+dy < 0 || y+dy > arr(0).size - 1){
        return '.'
    }
    val currVal = arr(x+dx)(y+dy)
    if("L#".contains(currVal)) {
        return currVal
    }
    return getAdjacent(dx, dy)(arr, x+dx, y+dy)
}

//literally fml so annoying
val u = getAdjacent(-1, 0) _
val d = getAdjacent(1, 0) _
val l = getAdjacent(0, -1) _
val r = getAdjacent(0, 1) _
val dl = getAdjacent(-1, -1) _
val dr = getAdjacent(-1, 1) _
val ur = getAdjacent(1, 1) _
val ul = getAdjacent(1, -1) _

def updateData(arr: Array[Array[Char]], x: Int, y: Int): Char = {
    val directionList = Array(d,u,l,r,dl,dr,ur,ul)
    val adjacentFields = directionList.map(_(arr, x, y))
    arr(x)(y) match {
        case 'L' => if(adjacentFields.count(_ == '#') == 0) '#' else 'L'
        case '#' => if(adjacentFields.count(_ == '#') >= 5) 'L' else '#'
        case '.' => '.'
    }
    
}

// utility

def logAndReturn(first: Array[Array[Char]], f: Array[Array[Char]] => Array[Array[Char]]): Int = {
   // least elegant but yolo
   val one = f(first)
   val two = f(one)
   val count = one.flatten.count(_ == '#') 
   println(count)
   if(one.flatten.count(_ == '#') == two.flatten.count(_ == '#')) count else logAndReturn(two, f)
}


val input = 
  Source.fromFile("input.txt")
        .getLines
        .toArray
        .map(_.toArray)

val q1 = logAndReturn(input, nextQ1)
val q2 = logAndReturn(input, nextQ2)


println("q1", q1)
println("q2", q2)


