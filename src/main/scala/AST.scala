trait AST {
  def getString() : String
  def getChildrenCount() : Int
  def getChildren() : List[AST]
  def createNext(node: AST) : AST = {
    this match {
      // currently, we expect binary operations to be the biggest
      case BinaryOperation(_, _, _) => ???
      case UnaryOperation(content, wrapped) => BinaryOperation(content, wrapped, node)
      case Token(content) => UnaryOperation(content, node)
    }
  }
}
case class BinaryOperation(content: String, left: AST, right: AST) extends AST {
  override def getString() = content
  override def getChildrenCount() = 2
  def getChildren() = {
    List(left, right)
  }
}
case class UnaryOperation(content: String, wrapped: AST) extends AST {
  override def getString() = content
  override def getChildrenCount() = 1
  def getChildren() = {
    List(wrapped)
  }
}
case class Token(content: String) extends AST {
  override def getString() = content
  override def getChildrenCount() = 0
  def getChildren() = {
    List()
  }
}
