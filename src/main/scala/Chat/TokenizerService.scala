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
          (spellCheckerSvc.getClosestWordInDictionary(word), getToken(word))
    }
    TokenizedImpl(tokens)

    /*
     take a string and return the corresponding token
     */
  def getToken(word: String): Token =
    if word == "bonjour" then BONJOUR
    else if word == "je" then JE
    else if word == "etre" then ETRE
    else if word == "vouloir" then VOULOIR
    else if word == "assoiffe" then ASSOIFFE
    else if word == "affame" then AFFAME
    else if word == "biere" then PRODUIT
    else if word == "croissant" then PRODUIT
    else if word == "et" then ET
    else if word == "ou" then OU
    else if word == "svp" then SVP
    else if word.startsWith("_") then PSEUDO
    else if word.matches("[0-9]+") then NUM
    else UNKNOWN
end TokenizerService
