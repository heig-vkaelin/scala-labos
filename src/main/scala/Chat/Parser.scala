package Chat

class UnexpectedTokenException(msg: String) extends Exception(msg) {}

class Parser(tokenized: Tokenized):
  import ExprTree._
  import Chat.Token._

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokenized.nextToken()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokenized.nextToken()

  /** "Eats" the expected token and returns it value, or terminates with an
    * error.
    */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts
    * arbitrarily many arguments of type Token
    */
  private def expected(token: Token, more: Token*): Nothing =
    expected(more.prepended(token))
  private def expected(tokens: Seq[Token]): Nothing =
    val expectedTokens = tokens.mkString(" or ")
    throw new UnexpectedTokenException(
      s"Expected: $expectedTokens, found: $curToken"
    )

  /** the root method of the parser: parses an entry phrase */
  def parsePhrases(): ExprTree =
    if curToken == BONJOUR then readToken()

    if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
      Price(parseProducts())
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      Price(parseProducts())
    else if curToken == JE then
      readToken()

      if curToken == ETRE then
        readToken()
        if curToken == ASSOIFFE then Thirsty
        else if curToken == AFFAME then Hungry
        else parseName()
      else if curToken == VOULOIR then
        readToken()
        if curToken == CONNAITRE then
          readToken()
          eat(MON)
          eat(SOLDE)
          Balance
        else if curToken == COMMANDER then
          readToken()
          Command(parseProducts())
        else expected(COMMANDER, CONNAITRE)
      else if curToken == ME then
        readToken()
        eat(APPELER)
        parseName()
      else expected(ETRE, VOULOIR, ME)
    else expected(BONJOUR, JE)

    /** Parse a name */
  def parseName() =
    val pseudo = eat(PSEUDO).substring(1)
    Identification(pseudo)

    /** Parse a single product */
  def parseProduct() =
    val quantity = eat(NUM).toInt
    val name = eat(PRODUIT)

    if curToken == MARQUE then
      val brand = eat(MARQUE)
      Product(name, Some(brand), quantity)
    else Product(name, None, quantity)

    /** Parse a potential list of products, separated by ET or OU */
  def parseProducts(): ExprTree =
    val product = parseProduct()

    def eval(exprTree: ExprTree): ExprTree =
      if curToken == ET then
        readToken()
        eval(And(exprTree, parseProduct()))
      else if curToken == OU then
        readToken()
        eval(Or(exprTree, parseProduct()))
      else exprTree
    end eval

    eval(product)
