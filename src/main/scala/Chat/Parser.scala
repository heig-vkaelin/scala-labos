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
  // TODO - Part 2 Step 4
  def parsePhrases(): ExprTree =
    if curToken == BONJOUR then readToken()

    if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
      Price(getProducts())
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
      Price(getProducts())
    else if curToken == JE then
      readToken()

      // Assoifé, affamé, pseudo
      if curToken == ETRE then
        readToken()
        if curToken == ASSOIFFE then Thirsty
        else if curToken == AFFAME then Hungry
        else getName()
      else if curToken == VOULOIR then
        readToken()
        if curToken == CONNAITRE then
          readToken()
          eat(MON)
          eat(SOLDE)
          Balance
        else if curToken == COMMANDER then Command(getProducts())
        else expected(COMMANDER, CONNAITRE)
      else if curToken == ME then
        readToken()
        eat(APPELER)
        getName()
      else expected(ETRE, VOULOIR, ME)
    else expected(BONJOUR, JE)

  def getName() =
    val pseudo = eat(PSEUDO).substring(1)
    Identification(pseudo)

  def getProduct() =
    val quantity = eat(NUM).toInt
    val name = eat(PRODUIT)

    if curToken == MARQUE then
      val brand = eat(MARQUE)
      Product(name, brand, quantity)
    else DefaultProduct(name, quantity)

  def getProducts(): ExprTree =
    val product = getProduct()

    readToken()

    if curToken == ET then And(product, getProducts())
    else if curToken == OU then Or(product, getProducts())
    else product
