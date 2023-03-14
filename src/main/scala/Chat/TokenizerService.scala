package Chat

import Chat.Token.*
import Utils.SpellCheckerService
import Utils.Dictionary.dictionary

class TokenizerService(spellCheckerSvc: SpellCheckerService):
  /** Separate the user's input into tokens
    * @param input
    *   The user's input
    * @return
    *   A Tokenizer which allows iteration over the tokens of the input
    */
  def tokenize(input: String): Tokenized =
    // remove all the punctuation marks, single quote and multiple spaces
    val inputWithoutPunctuation =
      input.replaceAll("[.,;:!?]", "").replaceAll("['\\s\\s+/g]", " ")
    val words = inputWithoutPunctuation.split(" ")

    val tokens = words.map { word =>
      dictionary.get(word) match
        case Some(value) => (value, getToken(value))
        case None =>
          val closestWord = spellCheckerSvc.getClosestWordInDictionary(word)
          (closestWord, getToken(closestWord))
    }
    TokenizedImpl(tokens)

    /*
     take a string and return the corresponding token
     */
  def getToken(word: String): Token =
    word match {
      case "bonjour"                   => BONJOUR
      case "je"                        => JE
      case "etre"                      => ETRE
      case "vouloir"                   => VOULOIR
      case "assoiffe"                  => ASSOIFFE
      case "affame"                    => AFFAME
      case "biere"                     => PRODUIT
      case "croissant"                 => PRODUIT
      case "et"                        => ET
      case "ou"                        => OU
      case "svp"                       => SVP
      case _ if word.startsWith("_")   => PSEUDO
      case _ if word.forall(_.isDigit) => NUM
      case _                           => UNKNOWN
    }
end TokenizerService
