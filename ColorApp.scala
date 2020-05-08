import ConSat._

object ColorApp {
  def main(args: Array[String]): Unit = {
    if (args.length != 2) throw usageError

    val alg: ConSat[ColorGraph] = args(0) match {
      case "dfs" => new DFS[ColorGraph]()
      case "fc" => new FC[ColorGraph]()
      case "mcv" => new MCV[ColorGraph]()
      case _ => throw usageError
    }

    val graph = ColorGraph.parseFromInput(args(1).toInt)

    val result = alg.find(graph)

    if (result.isDefined) print(result.get.print
      + "\n" + ColorGraph.nodesExplored + " branching nodes explored.")
    else print("No solution."
      + "\n" + ColorGraph.nodesExplored + " branching nodes explored.")

  }

  def usageError = new IllegalArgumentException(
    "Usage: run.sh <alg_name> <num_colors>. Valid alg names include " +
      "dfs, fc, and mcv.  Expects graph in DIMACS format on standard input")
}