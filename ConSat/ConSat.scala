package ConSat

trait ConSat[A <: ConSatable] {
  var stack: List[ConSatable] = List[A]()

  def find(searchable: A): Option[ConSatable] = {
    stack = searchable +: stack
    search()
  }
  protected def search(): Option[ConSatable]
}