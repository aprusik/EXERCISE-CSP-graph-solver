package ConSat

trait ConSatable {
  var wipeout: Boolean
  def hasViolation: Boolean
  def isViolation: Boolean
  def expand: List[ConSatable]
  def isComplete: Boolean
  def removeConflicts(): Boolean
  def moreConstrainedThan(that: ConSatable): Boolean
  def print: String
}