import scala.io.StdIn
import Utils._
import Chat.{TokenizerService, Token, UnexpectedTokenException, Parser, AnalyzerService}
import Data._

import scala.io.StdIn

object MainParser:
  /**
    * Convert the user input to lower case, then take an action depending on the value.
    *
    * @param tokenizerSvc The service used to tokenize the input
    * @param analyzerSvc The service used to analyze the expression tree
    * @param session The session instance that stores
    * @param input The user input
    * @return whether the input loop should continue or not
    */
  def evaluateInput(
            tokenizerSvc: TokenizerService, 
            analyzerSvc: AnalyzerService,
            session: Session,
        )(input: String): Boolean =
    input.toLowerCase match
        case "quitter" => 
          println("Adieu.")
          false  // close loop
        case "santé !" =>
          for i <- 2 to 6 do
            println(s"Nombre de *clinks* pour un santé de $i personnes : ${ClinksCalculator.calculateCombination(i, 2)}.")
          true  // continue loop
        case s =>
          try
            val tokenized = tokenizerSvc.tokenize(s)

            val parser = new Parser(tokenized)
            val expr = parser.parsePhrases()

            val printResult = analyzerSvc.reply(session)(expr)

            println(printResult)
          catch
            case e: UnexpectedTokenException => println(s"Invalid input. ${e.getMessage}")
          true  // continue loop
  end evaluateInput

  def main(args: Array[String]): Unit =
    val spellCheckerSvc: SpellCheckerService = new SpellCheckerImpl(Dictionary.dictionary)
    val tokenizerSvc: TokenizerService = new TokenizerService(spellCheckerSvc)
    val productSvc: ProductService = new ProductImpl()
    val accountSvc: AccountService = new AccountImpl()
    val sessionSvc: SessionService = new SessionImpl()
    val analyzerSvc: AnalyzerService = new AnalyzerService(productSvc, accountSvc)

    println("Bienvenue au Chill-Out !")

    val session = sessionSvc.create()

    while 
      print("> ")
      evaluateInput(tokenizerSvc, analyzerSvc, session)(StdIn.readLine)
    do ()
  end main
end MainParser

