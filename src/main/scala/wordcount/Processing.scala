package wordcount

class Processing {

  /** ********************************************************************************************
   *
   * Aufgabe 1
   *
   *  ** ******************************************************************************************
   */
  def getWords(line: String): List[String] = {
    /*
     * Extracts all words from a line
     * 
     * 1. Removes all characters which are not letters (A-Z or a-z)
     * 2. Shifts all words to lower case
     * 3. Extracts all words and put them into a list of strings
     */
    line.replaceAll("[^a-zA-Z]+", " ").toLowerCase().split(" ").toList
  }

  def getAllWords(l: List[(Int, String)]): List[String] = {
    /*
     * Extracts all words from a List containing line number and line tuples
     * The words should be in the same order as they occur in the source document
     * 
     * Hint: Use the flatMap function
     */
    l.flatMap(X => getWords(X._2)).filter(Y => Y != "")
  }

  def countWords(l: List[String]): List[(String, Int)] = {
    /*
     *  Gets a list of words and counts the occurrences of the individual words
     */
    (for (x <- l) yield (x, (for (y <- l if (x == y)) yield y).length)).distinct
  }

  /** * * ******************************************************************************************
   *
   * Aufgabe 2
   *
   * * * ******************************************************************************************
   */

  def getAllWordsWithIndex(l: List[(Int, String)]): List[(Int, String)] = (for (x <- l) yield (getWords(x._2).map(X => (x._1, X)))).flatten.filter(X => X._2 != "")

  def createInverseIndex(l: List[(Int, String)]): Map[String, List[Int]] = {
    val res = scala.collection.mutable.Map[String, List[Int]]()
    // hier geht auch groupMap
    for (x <- l) {
      res.update(x._2, x._1 :: res.getOrElse(x._2, List()))
    }
    res.mapValues(_.reverse).toMap
  }

  def orConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = (for (x <- words) yield invInd.get(x)).flatten.flatten.distinct

  def andConjunction(words: List[String], invInd: Map[String, List[Int]]): List[Int] = {
    val list = (for (x <- words) yield invInd.get(x)).flatten.flatten
    (for (x <- list if (list.filter(Y => Y == x).length == words.length)) yield x).distinct
  }
  }


  object Processing {

    def getData(filename: String): List[(Int, String)] = {
      val url = getClass.getResource("/" + filename).getPath
      val src = scala.io.Source.fromFile(url.replaceAll("%20", " "))
      val iter = src.getLines()
      var c = -1
      val result = (for (row <- iter) yield {
        c = c + 1; (c, row)
      }).toList
      src.close()
      result
    }
  }

