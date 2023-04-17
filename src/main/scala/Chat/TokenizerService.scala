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
    val words =
      input
        .replaceAll("[.,!?*]", "") // remove punctuation
        .replaceAll("[']", " ") // replace apostrophes
        .replaceAll("\\s{2,}", " ") // replace multiple spaces
        .trim()
        .split(" ")
        .filter(_.nonEmpty)

    val tokens = words.map { word =>
      dictionary.get(word) match
        case Some(value) => (value, getToken(value))
        case None =>
          val closestWord = spellCheckerSvc.getClosestWordInDictionary(word)
          (closestWord, getToken(closestWord))
    }
    TokenizedImpl(tokens)

  /** Get the token corresponding to the given word
    *
    * @param word
    *   the word to tokenize
    * @return
    *   the token corresponding to the given word
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
      case "quel"                      => QUEL
      case "le"                        => LE
      case "prix"                      => PRIX
      case "de"                        => DE
      case "combien"                   => COMBIEN
      case "couter"                    => COUTER
      case "commander"                 => COMMANDER
      case "connaitre"                 => CONNAITRE
      case "mon"                       => MON
      case "solde"                     => SOLDE
      case "me"                        => ME
      case "appeler"                   => APPELER
      case "maison"                    => MARQUE
      case "cailler"                   => MARQUE
      case "farmer"                    => MARQUE
      case "boxer"                     => MARQUE
      case "wittekop"                  => MARQUE
      case "punkipa"                   => MARQUE
      case "jackhammer"                => MARQUE
      case "tenebreuse"                => MARQUE
      case _ if word.startsWith("_")   => PSEUDO
      case _ if word.forall(_.isDigit) => NUM
      case _                           => UNKNOWN
    }
end TokenizerService
