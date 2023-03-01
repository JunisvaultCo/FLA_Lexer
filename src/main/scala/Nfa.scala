import scala.collection.IterableOnce.iterableOnceExtensionMethods

class Nfa[A](state0: A, stateF: Map[A, Boolean], states: Set[A], transitions: Map[(A, String), Set[A]]) {

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Nfa[B] = {
    val state0B = f(state0)
    val stateFB = stateF.map[B, Boolean]((a) => (f(a._1), a._2))
    val statesB = states.map(f)
    val transitionsB = transitions.map[(B, String), Set[B]]((a) => ((f(a._1._1), a._1._2), a._2.map(f)))
    new Nfa(state0B, stateFB, statesB, transitionsB) : Nfa[B]
  }

  def next(state:A, c: Char): Set[A] = {
    if (transitions.contains((state, c.toString)))
      transitions((state, c.toString))
    else
      Set[A]()
  }

  def accepts(str: String): Boolean = { // TODO implement accepts
    val word = str.toList
    var currentStates:Set[A] = Set(state0)
    for (x <- word) yield {
        val oldStates = currentStates
        currentStates = doEpsilonTransitions(oldStates)
        currentStates = (currentStates ++ oldStates).foldRight(Set[A]())((e, s) => s ++ realNext(e, x.toString))
    }
    currentStates = doEpsilonTransitions(currentStates)
    currentStates.exists(stateF(_))
  }

  def doEpsilonTransitions(s: Set[A]) : Set[A] = {
    val oldStatesQueue = scala.collection.mutable.Queue[A]()
    oldStatesQueue.enqueueAll(s)
    var rez = s
    // do all epsilon transitions and save them as part of the same state
    while (oldStatesQueue.nonEmpty) {
      val elem = oldStatesQueue.dequeue()
      for (x <- realNext(elem, "eps")) yield {
        if (!rez.contains(x)) {
          rez = rez ++ Set[A](x)
          oldStatesQueue.enqueue(x)
        }
      }
    }
    rez
  }

  def getStates : Set[A] = {
    states
  }

  def isFinal(state: A): Boolean = {
    stateF(state)
  }

  // mine

  def realNext(state:A, c: String): Set[A] = {
    if (transitions.contains((state, c)))
      transitions((state, c))
    else
      Set[A]()
  }

  def getState0(): A = state0
  def getStateF(): Map[A, Boolean] = stateF
  def getTransitions(): Map[(A, String), Set[A]] = transitions
  override def toString(): String = {
    "NFA[" + state0.getClass().toString +
    "](" +
    "state0: " + state0 +
    ", stateF: " + stateF +
    ", states: " + states +
    ", transitions: " + transitions +
    ")"
  }
}

// This is a companion object to the Nfa class. This allows us to call the method fromPrenex without instantiating the Nfa class beforehand.
// You can think of the methods of this object like static methods of the Nfa class
object Nfa {
  def fromPrenex(str: String): Nfa[Int] = {
    fromAST(getAST(str))
  }
  def fromAST(ast: AST): Nfa[Int] = {
    ast.getString() match {
      case "CONCAT" => {
        val nfa1 = fromAST(ast.getChildren().head)
        val nfa2 = fromAST(ast.getChildren().tail.head).map(_+nfa1.getStates.size)
        var newTransitions :Map[(Int, String), Set[Int]] = nfa1.getTransitions() ++ nfa2.getTransitions()
        val nfa1Final = nfa1.getStateF().foldRight(Set[Int]())((e, s) => if (e._2) s + (e._1) else s)
        newTransitions = newTransitions ++
              nfa1Final.foldRight(Map[(Int, String), Set[Int]]())((e, m) => {
                m ++ Map((e, "eps") -> Set(nfa2.getState0()))
              })
        val newStateF = nfa1.getStateF().map(a => (a._1, false)) ++ nfa2.getStateF()
        new Nfa(nfa1.getState0(), newStateF, nfa1.getStates ++ nfa2.getStates, newTransitions)
      }
      case "UNION" => {
        val nfa1 = fromAST(ast.getChildren().head).map(_+2)
        val nfa2 = fromAST(ast.getChildren().tail.head).map(_+nfa1.getStates.size + 2)
        val nfa1Final = nfa1.getStateF().foldRight(Set[Int]())((e, s) => if (e._2) s + (e._1) else s)
        val nfa2Final = nfa2.getStateF().foldRight(Set[Int]())((e, s) => if (e._2) s + (e._1) else s)
        val newStateF = nfa1.getStateF().map(a => (a._1, false)) ++ nfa2.getStateF().map(a => (a._1, false)) ++ Map(0 -> false, 1 -> true)
        var newTransitions :Map[(Int, String), Set[Int]] = nfa1.getTransitions() ++ nfa2.getTransitions()
        // add final transitions
        newTransitions = newTransitions ++
          (nfa1Final ++ nfa2Final).foldRight(Map[(Int, String), Set[Int]]())((e, m) => {
          m ++ Map((e, "eps") -> Set(1))
        })
        // add start transitions
        newTransitions = newTransitions ++ Map((0, "eps")->Set(nfa1.getState0(), nfa2.getState0()))

        new Nfa(0, newStateF, nfa1.getStates ++ nfa2.getStates ++ Set(0, 1), newTransitions)
      }
      case "STAR" => {
        val nfa1 = fromAST(ast.getChildren().head).map(_+2)
        val nfa1Final = nfa1.getStateF().foldRight(Set[Int]())((e, s) => if (e._2) s + (e._1) else s)
        val newStateF = nfa1.getStateF().map(a => (a._1, false)) ++ Map(0 -> false, 1 -> true)
        var newTransitions:Map[(Int, String), Set[Int]] = nfa1.getTransitions()
        newTransitions = newTransitions ++ Map[(Int, String), Set[Int]]((0, "eps") -> Set(1, nfa1.getState0()))
        newTransitions = newTransitions ++ (nfa1Final.foldRight(Map[(Int, String), Set[Int]]())((e, m) => {
                                m ++ Map((e, "eps") -> Set(1, nfa1.getState0()))}))
        new Nfa(0, newStateF, nfa1.getStates ++ Set(0, 1), newTransitions)
      }
      case "PLUS" =>  {
        fromAST(BinaryOperation("CONCAT", ast.getChildren().head, UnaryOperation("STAR", ast.getChildren().head)))
      }
      case "MAYBE" => {
        fromAST(BinaryOperation("UNION", ast.getChildren().head, Token("eps")))
      }
      case "eps" => {
        new Nfa[Int](0, Map(0 -> false, 1 -> true), Set(0, 1), Map((0, "eps") -> Set(1)))
      }
      case "void" => {
        new Nfa[Int](0, Map(0 -> false), Set(0), Map())
      }
      case s => {
        new Nfa[Int](0, Map(0 -> false, 1 -> true), Set(0, 1), Map((0, s.charAt(0).toString) -> Set(1)))
      }
    }
  }
  // You can add more methods to this object
  def getTermList(str:String): List[List[Char]] = {
    var terms = List[List[Char]]()
    var escaped = false;
    var current = List[Char]()
    for (a <- str) yield {
      if (a == '\'') escaped = !escaped
      else if (a == ' ' && !escaped) {
        terms = terms ++ List[List[Char]](current)
        current = List[Char]()
      }
      else current = current ++ List[Char](a)
    }
    if (escaped)
      current = List('\'')
    terms ++ List[List[Char]](current)
  }
  def getAST(str: String) : AST = {
    val s = scala.collection.mutable.Stack[AST]()
    val termList = getTermList(str)
    for (x <- termList) yield {
      val xStr = String.copyValueOf(x.toArray)
      if (getOperandCount(xStr) == 0) {
        var current = Token(xStr) : AST
        while (s.nonEmpty && getOperandCount(s.top.getString()) - 1 == s.top.getChildrenCount()) {
          current = s.top.createNext(current)
          s.pop
        }
        if (s.nonEmpty) {
          current = s.top.createNext(current)
          s.pop
        }
        s.push(current)
      } else {
        s.push(Token(xStr))
      }
    }
    s.top
  }
  def getOperandCount(str: String): Int = {
    if (str.equals("UNION") || str.equals("CONCAT")) 2
    else if (str.equals("PLUS") || str.equals("STAR") || str.equals("MAYBE")) 1
    else 0
  }
}
