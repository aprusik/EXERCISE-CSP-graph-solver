package ConSat

import scala.annotation.tailrec

class DFS[A <: ConSatable]() extends ConSat[A] {
  @tailrec
  final override protected def search(): Option[ConSatable] = {
    if (stack.isEmpty) Option.empty
    else if (stack.head.isComplete)
      Some(stack.head)
    else {
      val children = stack.head.expand
      stack = stack.tail
      if (children.nonEmpty)
        for (child <- children) stack = child +: stack
      search()
    }
  }
}
