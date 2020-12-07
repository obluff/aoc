import scala.io.Source

case class graphEdge(edgeName: String, edgeVal: Int)

def extractBags(x: String): (String, List[graphEdge]) = {
    val bagPat = "(\\d+)? ([a-z]+ [a-z]+ bag)".r
    val firstBag = "([a-z]+ [a-z]+ bag)".r
    val node = firstBag.findFirstIn(x).getOrElse("")

    if(x.contains("no other bag")) {
        return (node, List(graphEdge("no other bag", 0)))
    }
    val edges = bagPat.findAllIn(x)
                      .matchData.map(_.subgroups)
                      .map(x => graphEdge(x(1), x(0).toInt))
                      .toList
    return node -> edges
}

def search(item: String,
           graphNodes: Map[String, List[graphEdge]],
           target: String,
           visited: Set[String]=Set()): Boolean = {
    if(!graphNodes.contains(item)) return false
    if(item == "no other bags") return false
    if(item  == target) return true
    val currComponents = graphNodes(item).map(_.edgeName)
    if(currComponents.toSet.contains(target)) return true
    return currComponents.map(search(_, graphNodes, target, visited | Set(item)))
                         .filter(_ == true).size > 0
}

def numKids(item: String, graphNodes: Map[String, List[graphEdge]]): Int = {
    if(!graphNodes.keys.toSet.contains(item)) return 0
    if(item == "no other bags") return 0
    val currComponents = graphNodes(item)
    return currComponents.map(_.edgeVal).sum +
           currComponents.map(e => e.edgeVal * (numKids(e.edgeName, graphNodes))).sum
}

val graph =
  Source.fromFile("input.txt")
         .getLines
         .map(x => extractBags(x.replace("bags", "bag")))
         .toMap

val q1 = graph.keys.toList.count(search(_, graph, "shiny gold bag"))
val q2 = numKids("shiny gold bag", graph)

println(q1)
println(q2)
