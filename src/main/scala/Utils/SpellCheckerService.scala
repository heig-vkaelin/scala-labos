package Utils

import scala.math.min

trait SpellCheckerService:
  /** This dictionary is a Map object that contains valid words as keys and
    * their normalized equivalents as values (e.g. we want to normalize the
    * words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /** Calculate the Levenstein distance between two words.
    * @param s1
    *   the first word
    * @param s2
    *   the second word
    * @return
    *   an integer value, which indicates the Levenstein distance between "s1"
    *   and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /** Get the syntactically closest word in the dictionary from the given
    * misspelled word, using the "stringDistance" function. If the word is a
    * number or a pseudonym, this function just returns it.
    * @param misspelledWord
    *   the mispelled word to correct
    * @return
    *   the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String])
    extends SpellCheckerService:

  // TODO: ça dégage ?
  def stringDistance_INCOMPREHENSIBLE(s1: String, s2: String): Int =
    ((0 to s2.length).toList /: s1)((prev, x) =>
      (prev zip prev.tail zip s2).scanLeft(prev.head + 1) {
        case (h, ((d, v), y)) =>
          min(min(h + 1, v + 1), d + (if (x == y) 0 else 1))
      }
    ).last

  // TODO: on garde ça ?

  def stringDistance(s1: String, s2: String): Int = {
    val memo = scala.collection.mutable.Map[(String, String), Int]()

    def levensthein(s1: String, s2: String): Int = {
      if (memo.contains((s1, s2)) == false)
        memo((s1, s2)) =
          if (s1.isEmpty) s2.length
          else if (s2.isEmpty) s1.length
          else {
            val cost = if (s1.head == s2.head) 0 else 1
            val deletion = levensthein(s1.tail, s2) + 1
            val insertion = levensthein(s1, s2.tail) + 1
            val substitution = levensthein(s1.tail, s2.tail) + cost
            List(deletion, insertion, substitution).min
          }
      memo((s1, s2))
    }
    levensthein(s1, s2)
  }

  def getClosestWordInDictionary(misspelledWord: String): String =
    misspelledWord match {
      case word if word.forall(_.isDigit) => word
      case word if word.startsWith("_")   => word
      case _ =>
        dictionary.minBy((key, _) => stringDistance(key, misspelledWord))._2
    }
end SpellCheckerImpl
