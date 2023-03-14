package Chat

import Chat.Token.*
import Utils.SpellCheckerService

trait Tokenized:
  /** Get the next token of the user input, or EOL if there is no more token.
    * @return
    *   a tuple that contains the string value of the current token, and the
    *   identifier of the token
    */
  def nextToken(): (String, Token)

class TokenizedImpl(val tokens: Array[(String, Token)]) extends Tokenized:
  var counter = 0
  def nextToken(): (String, Token) =
    if counter < tokens.length then
      val token = tokens(counter)
      counter += 1
      token
    else ("EOL", EOL)

end TokenizedImpl
