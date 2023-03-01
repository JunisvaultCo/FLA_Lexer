import java.io.{File, FileWriter}
import java.util.Scanner

class Dfa[A] (state0: A, stateF: Map[A, Boolean], states: Set[A], transitions: Map[(A, String), A], sink: A){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = {
    val state0B = f(state0)
    val stateFB = stateF.map[B, Boolean]((a) => (f(a._1), a._2))
    val statesB = states.map(f)
    val transitionsB = transitions.map[(B, String), B]((a) => ((f(a._1._1), a._1._2), f(a._2)))
    new Dfa(state0B, stateFB, statesB, transitionsB, f(sink))
  }

  def next(state:A, c: Char): A = {
      if (transitions.contains((state, c.toString)))
        transitions((state, c.toString))
      else sink
  }

  def accepts(str: String): Boolean = {
    val word = str.toList
    var currentState = state0
    for (x <- word) yield {
      currentState = next(currentState, x)
    }
    isFinal(currentState)
  }

  def getStates : Set[A] = {
    states
  }

  def getState0 : A = {
    state0
  }

  def isFinal(state: A): Boolean = {
    stateF.getOrElse(state, false)
  }

  override def toString(): String = {
    "DFA[" + state0.getClass().toString +
      "](" +
      "state0: " + state0 +
      ", stateF: " + stateF +
      ", states: " + states +
      ", transitions: " + transitions +
      ", sink: " + sink + ")"
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {
  def fromPrenex(str: String): Dfa[Int] = {
    val nfa = Nfa.fromPrenex(str)
    fromNfa(nfa, 0, 1, (f:Int, b:Set[Int]) => f+1)
  }
  def fromNfa[A,B](nfa: Nfa[A], sink : B, state0_i : B, nextF: (B, Set[A]) => B): Dfa[B] = {
    var stateF = Map[B, Boolean]()
    val state0T = nfa.doEpsilonTransitions(Set(nfa.getState0()))
    val state0 = nextF(state0_i, state0T)
    var states = Set[B](sink, state0)
    var transitions = Map[(B, String), B]()
    val queue = scala.collection.mutable.Queue[B]()
    queue.enqueue(state0)
    var newStateMapping = Map[B, Set[A]](state0 -> state0T, sink -> Set[A]())
    var revStateMapping = Map[Set[A], B](state0T -> state0, Set[A]() -> sink)

    var alphabet = Set[String]()
    for (x <- nfa.getTransitions()) yield {
      alphabet = alphabet ++ Set[String](x._1._2)
    }
    alphabet = alphabet.filter(a => a != "eps")
    var last = state0
    var steps = 0
    while (queue.nonEmpty) {
      steps = steps + 1
      val current = queue.dequeue
      // for each character, see next transition
      for (c <- alphabet) yield {
        var transitionStateElements = Set[A]()
        for (y <- newStateMapping(current)) yield {
          if (nfa.getTransitions().contains((y, c))) {
            transitionStateElements = transitionStateElements ++ nfa.getTransitions()((y, c))
          }
        }
        // link to sink and ignore everything else
        if (transitionStateElements.isEmpty) {
          transitions = transitions ++ Map((current, c) -> sink)
        } else {
          transitionStateElements = nfa.doEpsilonTransitions(transitionStateElements)
          if (revStateMapping.contains(transitionStateElements)) {// transition state exists
            transitions = transitions + ((current, c) -> revStateMapping(transitionStateElements))
          } else {
            val newTransitionState = nextF(last, transitionStateElements)
            last = newTransitionState
            states = states + newTransitionState
            transitions = transitions + ((current, c) -> newTransitionState)
            newStateMapping ++= Map(newTransitionState -> transitionStateElements)
            revStateMapping ++= Map(transitionStateElements -> newTransitionState)
            queue.enqueue(newTransitionState)
          }
        }
      }
    }
    // do new stateF
    stateF = states.foldRight(Map[B, Boolean]())((v, m) => m + (v -> newStateMapping(v).exists(nfa.getStateF()(_))))
    new Dfa(state0, stateF, states, transitions, sink)
  }

}
