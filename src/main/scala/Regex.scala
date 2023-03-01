import scala.collection.mutable

object Regex {
  /*
    This function should:
    -> Classify input as either character(or string) or operator
    -> Convert special inputs like [0-9] to their correct form
    -> Convert escaped characters
  */
  def preprocess(s:List[Char]): List[Either[Char,Char]] = {
    var escaped = false
    var gotEscapedChar = false
    var interval = false
    var first = 'c'
    var second = 'd'
    var isSecondTerm = false
    var result = List[Either[Char,Char]]()
    var nestingLevel = 0

    for (x <- s) yield {
      if (!escaped && x == '\'') escaped = true
      else if (escaped && (!gotEscapedChar || x != '\'')) {
        gotEscapedChar = true
        result = result ++ List[Either[Char,Char]](Left(x))
      } else if (x == '\'') {
        escaped = false
        gotEscapedChar = false
      } else if (interval && !isSecondTerm) {
        first = x
        isSecondTerm = true
      } else if (interval && x == '-') {
      } else if (interval && x == ']') {
        isSecondTerm = false
        interval = false
        result = result ++ List[Either[Char,Char]](Right('('), Left(first))
        for (i <- (first + 1) to second) {
          result = result ++ List[Either[Char,Char]](Right('|'), Left(i.toChar))
        }
        result = result ++ List[Either[Char,Char]](Right(')'))
      } else if (interval) second = x
      else if (x == '[') interval = true
      else if (x == '(' || x == ')' || x == '|' || x == '*') {
        if (x == '(') nestingLevel = nestingLevel + 1
        if (x == ')') nestingLevel = nestingLevel - 1
        result = result ++ List[Either[Char,Char]](Right(x))
      } else if (x == '?') {
        // we need to retrieve the last term
        var lastTerm = List[Either[Char,Char]]()
        var reverseNestingLevel = 0
        var done = false
        var sawToken = false;
        for (x <- result.reverse) {
          val content = x.merge
          if (done) {}
          else {
            if (x.isRight && (content == ')' || content == '(')) {
              if (content == ')') reverseNestingLevel = reverseNestingLevel - 1
              else reverseNestingLevel = reverseNestingLevel + 1
            } else if (x.isLeft) sawToken = true
            if (!done)
              lastTerm = List[Either[Char,Char]](x) ++ lastTerm
            if (reverseNestingLevel == 0 && sawToken) {
              done = true
            }
          }
        }
        result = result.dropRight(lastTerm.size)
        lastTerm = List[Either[Char,Char]](Right('(')) ++ lastTerm
        result = result ++ lastTerm ++
          List[Either[Char,Char]](Right('|'), Right('('), Left('e'), Left('p'), Left('s'), Right(')'), Right(')'))
      } else if (x == '+') {
        // we need to retrieve the last term
        var lastTerm = List[Either[Char,Char]]()
        var reverseNestingLevel = 0
        var done = false
        var sawToken = false;
        for (x <- result.reverse) {
          val content = x.merge
          if (done) {}
          else {
            if (x.isRight && (content == ')' || content == '(')) {
              if (content == ')') reverseNestingLevel = reverseNestingLevel - 1
              else reverseNestingLevel = reverseNestingLevel + 1
            } else if (x.isLeft) sawToken = true
            if (!done)
              lastTerm = List[Either[Char,Char]](x) ++ lastTerm
            if (reverseNestingLevel == 0 && sawToken) {
              done = true
            }
          }
        }
        result = result.dropRight(lastTerm.size)
        lastTerm = List(Right('(')) ++ lastTerm ++ lastTerm
        result = result ++ lastTerm ++ List[Either[Char,Char]](Right('*'), Right(')'))
      } else {
        result = result ++ List[Either[Char,Char]](Left(x))
      }
    }
    result
  }

  def stringToList(str : String) : List[Char] = str.foldRight[List[Char]](List[Char]())((a, l : List[Char]) => l.::(a))

  def stringToLeftList(str : String) : Either[List[Char], List[Char]] = Left(str.foldLeft(List[Char]())((l, c) => {l ++ List(c)}))

  def eitherListToString(l: Either[List[Char], List[Char]]) = {
    val rez = l.merge.foldLeft("")((str, c) => str + c)
    if (rez.equals(" "))
      "' '"
    else if (rez.equals("'"))
      "'''"
    else if (rez.equals("CONCAT CONCAT e p s"))
      "'eps'"
    else
      rez
  }

  def attemptStack(stack: mutable.Stack[Either[List[Char],List[Char]]]) : Unit = {
    var term = List[Either[List[Char], List[Char]]]()
    while (stack.nonEmpty && !(stack.top.isRight && stack.top.merge == List('('))) {
      term = List(stack.top) ++ term
      stack.pop
    }
    if (stack.nonEmpty)
      stack.pop
    // first, do stars
    var auxTerm = List[Either[List[Char], List[Char]]]()
    var previous = Left(List[Char]()) : Either[List[Char], List[Char]]
    for (y <- term) {
      if (y.isRight && y.merge == List('*')) {
        auxTerm = auxTerm.dropRight(1)
        previous = stringToLeftList("STAR " + eitherListToString(previous))
      } else
        previous = y
      auxTerm = auxTerm ++ List(previous)
    }
    term = auxTerm
    auxTerm = List[Either[List[Char], List[Char]]]()
    previous = Left(List[Char]()) : Either[List[Char], List[Char]]
    // then do concats
    for (y <- term) {
      if (y.isLeft && previous.isLeft) {
        if (previous.merge.nonEmpty) {
          auxTerm = auxTerm.dropRight(1)
          previous = stringToLeftList("CONCAT " + eitherListToString(previous) + " " + eitherListToString(y))
        } else
          previous = y
      } else {
        previous = y
      }
      auxTerm = auxTerm ++ List(previous)
    }
    term = auxTerm
    // then do unions
    auxTerm = List[Either[List[Char], List[Char]]]()
    previous = Left(List[Char]()) : Either[List[Char], List[Char]]
    var previous2 = previous
    for (y <- term) {
      val aux = previous
      if (previous.isRight && previous.merge == List('|')) {
        auxTerm = auxTerm.dropRight(2) // get rid of | and what was before it
        previous = stringToLeftList("UNION " + eitherListToString(previous2) + " " + eitherListToString(y))
      } else
        previous = y
      previous2 = aux
      auxTerm = auxTerm ++ List(previous)
    }
    term = auxTerm
    stack.pushAll(term)
  }

  // This function should construct a prenex expression out of a normal one.
  def toPrenex(str: String): String = {
    val stack = mutable.Stack[Either[List[Char], List[Char]]]()
    for (x <- preprocess(stringToList(str))) yield {
      val content = x.merge
      if (x.isRight && content == ')') {
        attemptStack(stack)
      } else {
        if (x.isRight)
          stack.push(Right(stringToList(content + "")))
        else
          stack.push(Left(stringToList(content + "")))
      }
    }
    attemptStack(stack)
    eitherListToString(stack.top)
  }
}
