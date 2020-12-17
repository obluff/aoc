case class point(data: Seq[Int], active: Boolean=false) {
   override def toString() = data.mkString(",") ++ "," ++ active.toString
   val - = (p: point) => point((data zip p.data).map(x => x._1 - x._2), active)
   val + = (p: point) => point((data zip p.data).map(x => x._1 + x._2), active)
   val activate = () => point(data, true)
   val deactivate = () => point(data, false)
}

/* tbh got this cartesianProduct implementatino from stackoverflow but i got the original idea 
   for the cartesian product from dave's day 11 answer, thanks dave! */
def cartesianProduct[T](seqs: Seq[Seq[T]]): Seq[Seq[T]] = {
  seqs.foldLeft(Seq(Seq.empty[T]))((b, a) => b.flatMap(i => a.map(j => i ++ Seq(j))))
}

val getDeltaPoints = (dim: Int) => {
    val combinations = cartesianProduct((1 to dim).map(_ => Seq(-1,0,1))).map(point(_, true)).toSet 
    val identityPoint = point((1 to dim).map(_ => 0), true)
    combinations diff Set(identityPoint)
}

def getAllNeighbors(p: point, deltas: Set[point]): Set[point] = {
     deltas.map(p + _)
           .map(_.activate()).toSet   
}
def activeNeighbors(p: point, cells: Set[point], deltas: Set[point]): Int = {
    (cells intersect getAllNeighbors(p, deltas)).size
}

def getNext(deltas: Set[point])(activePoints: Set[point]): Set[point] = {
    val otherNodes =  activePoints.map(getAllNeighbors(_, deltas) diff activePoints).flatten.map(_.deactivate())
    val allPoints = otherNodes | activePoints
    allPoints.map{x => 
        val numNeighbors= activeNeighbors(x, activePoints, deltas)
        x.active match {
            case true => if(numNeighbors == 2 || numNeighbors == 3) x else x.deactivate()
            case false => if(numNeighbors == 3) x.activate() else x
        }
    }.filter(_.active)
}

val initialState = 
"""##....#.
#.#..#..
...#....
...#.#..
###....#
#.#....#
.#....##
.#.###.#"""

val clean = initialState.split("\n")
val m = clean(0).size
val indexed = clean.flatten.zipWithIndex
val inputQ1= indexed.map(x => point(Seq(x._2 / m, x._2 % m, 0), x._1 == '#')).toSet.filter(_.active)
val inputQ2= indexed.map(x => point(Seq(x._2 / m, x._2 % m, 0, 0), x._1 == '#')).toSet.filter(_.active)

val threeD = getNext(getDeltaPoints(3)) _
val fourD = getNext(getDeltaPoints(4)) _
val f1 = (1 to 6).map(_ => threeD(_)).reduceLeft(_ andThen _)
val f2 = (1 to 6).map(_ => fourD(_)).reduceLeft(_ andThen _)

println(f1(inputQ1).size)
println(f2(inputQ2).size)
