import ConSat.ConSatable

import scala.collection.mutable
import scala.io.StdIn
import scala.util.Random

class ColorGraph(numVerts: Int, numColors: Int, var depth: Int = 0,
                 _edges: Option[mutable.ArraySeq[List[Int]]] = None,
                 _colors: Option[mutable.ArraySeq[Int]] = None,
                 _availColors: Option[mutable.ArraySeq[Set[Int]]] = None
                ) extends ConSatable {

  var edges: mutable.ArraySeq[List[Int]] =
    if (_edges.isEmpty)
      new mutable.ArraySeq[List[Int]](numVerts).map(_ => List[Int]())
    else _edges.get
  var colors: mutable.ArraySeq[Int] =
    if (_colors.isEmpty) new mutable.ArraySeq[Int](numVerts).map(_ => 0)
    else _colors.get
  var availColors: mutable.ArraySeq[Set[Int]] =
    if (_availColors.isEmpty)
      new mutable.ArraySeq[Set[Int]](numVerts).map(_ => (1 to numColors).toSet)
    else _availColors.get
  var wipeout: Boolean = false

  def addEdge(v1: Int, v2: Int): Unit = {
    edges(v1) = v2 +: edges(v1)
    edges(v2) = v1 +: edges(v2)
  }

  private def copy: ColorGraph = {
    new ColorGraph(numVerts, numColors,
      depth, Some(edges), Some(colors.clone()), Some(availColors.clone()))
  }

  private def setColor(color: Int): ColorGraph = {
    colors(depth) = color
    depth += 1
    this
  }

  override def hasViolation: Boolean = {
    var violation = false
    for (vert <- edges.indices; edge <- edges(vert); if !violation)
      violation = colors(vert) != 0 && colors(vert) == colors(edge)
    violation
  }

  override def isViolation: Boolean = {
    var violation = false
    for (edge <- edges(depth-1); if !violation)
      violation = colors(depth-1) != 0 && colors(depth-1) == colors(edge)
    violation
  }

  override def expand: List[ColorGraph] = {
    ColorGraph.nodesExplored += 1
    var child: ColorGraph = null
    for (color <- numColors to 1 by -1
         if {
           child = copy.setColor(color)

           !child.isViolation && availColors(depth).contains(color) //what about variable choice expansion: pick a new depth each time dependent on its constraint
         })
      yield child
  }.toList

  override def isComplete: Boolean = !colors.contains(0) && !hasViolation

  override def removeConflicts(): Boolean = {
    for (edge <- edges(depth - 1))
      availColors(edge) = availColors(edge) - colors(depth - 1)
    wipeout = availColors.exists((s: Set[Int]) => s.isEmpty) // returns false on domain wipeout
    !wipeout
  }

  // method designed with SCALA inheritance assistance from:
  // https://stackoverflow.com/questions/54792791/scala-abstract-comparison-method-in-trait/54793535#54793535
  override def moreConstrainedThan(that: ConSatable): Boolean = that match {
    case that: ColorGraph =>
      if (depth < availColors.size) {
        val c1 = availColors(depth).size
        val c2 = that.availColors(that.depth).size
        //      if (c1 == c2) Random.nextBoolean()
        //      else
        c1 < c2
      } else false
    case _ =>
      throw new UnsupportedOperationException("Unsupported Comparison types.")
  }

  override def print: String = {
    var s = s"s col $numColors"
    for (i <- colors.indices) s += s"\nl " + (i+1) + " " + colors(i)
    s
  }
}

object ColorGraph {
  var nodesExplored = 0

  def parseFromInput(numColors: Int): ColorGraph = {

    var newGraph: Option[ColorGraph] = Option.empty

    var edges = 1
    while (0 < edges) {
      val line: List[Any] = StdIn.readLine.split(" ").toList.map(
        (input: String) => {try input.toInt catch {case _: Exception => input}}
      )

      line match {
        // comment line; ignore
        case "c" :: _ => Unit
        // prepare line; declare vertices and edges
        case "p" :: "edge" :: (numVerts: Int) :: (numEdges: Int) :: _ =>
          edges = numEdges
          newGraph = Some(new ColorGraph(numVerts, numColors))
        case "p" :: _ =>
          throw formatException("Usage: p edge <num_vertices> <num_edges>")
        // edge line; declare edge points
        case "e" :: (v1: Int) :: (v2: Int) :: _ =>
          if (newGraph.isDefined) {
//            if (!newGraph.get.edges(v2-1).contains(v1))
              newGraph.get.addEdge(v1 - 1, v2 - 1)
            edges -= 1
          }
          else throw formatException("Must use p to define number of vertices " +
            "and edges before defining an edge.")
        case "e" :: _ => throw formatException("Usage: e <vertex_1> <vertex_2>")
        // extra case; line does not begin with c, p, or e
        case _ =>
          throw formatException("Only supports lines beginning with c, p, and e")
      }
    }

    if (newGraph.isDefined) newGraph.get
    else throw formatException("Graph not defined")
  }

  private def formatException(msg: String): IllegalArgumentException =
    new IllegalArgumentException("Input must be in standard DIMACS graph" +
      "format.  " + msg)
}