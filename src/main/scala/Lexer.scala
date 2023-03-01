case class Lexer (spec: String) {

  def parseSpecs() : (List[Nfa[(String,Int)]], Map[String, Int], Map[Int, String]) = {
    var map = Map[String, Int]()
    var rmap = Map[Int, String]()
    var list = List[Nfa[(String,Int)]]()
    val tokens = spec.split(";\n")
    var pos = 0
    for (y <- tokens) yield {
      var x = y.replaceAll("\\\\n", "\n")
      x = x.replaceAll("\\\\t", "\t")
      if (!x.isEmpty) {
        val name = x.split(": ").head
        val regex = x.split(": ").last
        list = list ++ List(Nfa.fromPrenex(Regex.toPrenex(regex)).map(f => (name, f)))
        map = map ++ Map(name -> pos)
        rmap = rmap ++ Map(pos -> name)
        pos = pos + 1
      }
    }
    (list, map, rmap)
  }
  def mapLocation(word: String) : Map[Int, (Int, Int)] = {
    var map = Map[Int, (Int, Int)]()
    var line = 0
    var charOnLine = 0
    var pos = 0
    for (x <- word) yield {
      if (x == '\n') {
        line = line + 1
        charOnLine = 0
      } else
        charOnLine = charOnLine + 1
      map = map ++ Map(pos -> (line, charOnLine))
      pos = pos + 1
    }
    map = map ++ Map(pos -> (line, charOnLine))
    map
  }
  /*
    This is the main function of the lexer, it splits the given word into a list of lexems
    in the format (LEXEM, TOKEN)
  */
  def lex(word: String): Either[String,List[(String,String)]] = {
    val parsedSpecs = parseSpecs()
    val nfas = parsedSpecs._1
    val indexes = parsedSpecs._2
    val rindexes = parsedSpecs._3
    val state0: (String, Int) = ("-1", -1)
    val stateF = nfas.foldRight(Map[(String, Int), Boolean]())((f, acc) => f.getStateF() ++ acc) ++ Map(state0 -> false)
    val states = nfas.foldRight(Set[(String, Int)]())((f, acc) => f.getStates ++ acc) ++ Set(state0)
    val states0: Set[(String, Int)] = nfas.foldRight(Set[(String, Int)]())((f, acc) => Set(f.getState0()) ++ acc)
    val newTransitions: Map[((String, Int), String), Set[(String, Int)]] = Map((state0, "eps") -> states0)
    val transitions =
        nfas.foldRight(Map[((String, Int), String), Set[(String, Int)]]())((f, acc) => f.getTransitions() ++ acc) ++ newTransitions
    val ultimateNfa = new Nfa(state0, stateF, states, transitions)
    val sink = (10000002,-2)
    var dfaState0 = (10000001,-1)
    val dfa = Dfa.fromNfa[(String, Int), (Int, Int)](ultimateNfa, sink, dfaState0, (a, b) => {
     // b
      (b.foldRight(10000000)((p, found) =>
        if (stateF.getOrElse(p, false) && found > indexes(p._1))
          indexes(p._1) else found)
        , a._2 + 1)
    })
    dfaState0 = dfa.getState0
    var result = List[(String, String)]()
    var currentPos = 0
    var lastPos = -1
    var accepted = ""
    var acceptedBy = ""
    var current = ""
    var currentState = dfaState0
    var ok = true
    var error = ""
    val mapLoc = mapLocation(word)
    while ((currentPos < word.length || !dfa.isFinal(currentState)) && ok) {
      if (currentState == sink || (!dfa.isFinal(currentState) && currentPos == word.length && current.nonEmpty)) {
        if (acceptedBy.isEmpty) {
          ok = false
          val loc = mapLoc.getOrElse(currentPos, (0, 1))
          if (currentPos == word.length && currentState != sink) {
            error = "No viable alternative at character EOF, line " + loc._1
          } else {
            val loc = mapLoc.getOrElse(currentPos - 2, (0, 0))
            error = "No viable alternative at character " + loc._2 + ", line " + loc._1
          }
        } else {
          result = result ++ List[(String, String)]((accepted, acceptedBy))
          accepted = ""
          acceptedBy = ""
          currentPos = lastPos
          current = ""
          currentState = dfaState0
        }
      }
      if (dfa.isFinal(currentState)) {
        lastPos = currentPos
        accepted = current
        acceptedBy = /*currentState.foldRight("")((p, found) =>
             if (stateF.getOrElse(p, false) && indexes.getOrElse(found,1000000) > indexes(p._1))
               p._1 else found)*/
             rindexes(currentState._1)
      }
      if (currentPos < word.length) {
        currentState = dfa.next(currentState, word.charAt(currentPos))
        current = current + word.charAt(currentPos)
        currentPos = currentPos + 1
      }
    }
    if (dfa.isFinal(currentState)) {
      accepted = current
      acceptedBy = /*currentState.foldRight("")((p, found) =>
          if (stateF.getOrElse(p, false) && indexes.getOrElse(found,1000000) > indexes(p._1))
            p._1 else found)*/
      rindexes(currentState._1)
      result = result ++ List[(String, String)]((accepted, acceptedBy))
    } else if (current.nonEmpty && ok) {
      val loc = mapLoc.getOrElse(currentPos, (0, 0))
      ok = false
      error = "No viable alternative at character EOF, line " + loc._1
    }
    if (!ok) {
      Left(error)
    } else
      Right(result)
  }
}