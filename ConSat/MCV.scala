package ConSat

import scala.annotation.tailrec

class MCV[A <: ConSatable]() extends ConSat[A] {
  @tailrec
  final override protected def search(): Option[ConSatable] = {
    if (stack.isEmpty) Option.empty
    else if (stack.head.isComplete)
      Some(stack.head)
    else {
      var children = stack.head.expand
      stack = stack.tail
//      children = children.sortWith((c1: ConSatable, c2: ConSatable) => {
//        c1.removeConflicts()
//        c2.removeConflicts()
//        c2.moreConstrainedThan(c1)
//      })
      if (children.nonEmpty) {
        var childStack: List[ConSatable] = Nil
        for (child <- children) {
          if (child.removeConflicts())
            childStack = child +: childStack
        }
        childStack = childStack.sortWith((c1: ConSatable, c2: ConSatable) => c1.moreConstrainedThan(c2))
        stack = childStack ++ stack
      }

//      stack = stack.sortWith((c1: ConSatable, c2: ConSatable) => {
//        c1.removeConflicts()
//        c2.removeConflicts()
//        c2.moreConstrainedThan(c1)})
      search()
    }
  }

}
