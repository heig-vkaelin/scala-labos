package Chat
import Data.{AccountService, ProductService, Session}
import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import ExprTree._

  val TOO_POOR =
    "Vous n'avez pas assez d'argent pour effectuer cette commande ! Vous êtes pauvre (:"

  /** Compute the price of the current node, then returns it. If the node is not
    * a computational node, the method returns 0.0. For example if we had a "+"
    * node, we would add the values of its two children, then return the result.
    * @return
    *   the result of the computation
    */
  def computePrice(t: ExprTree): Double = t match
    case And(left, right) => computePrice(left) + computePrice(right)
    case Or(left, right)  => Math.min(computePrice(left), computePrice(right))
    case Command(expr)    => computePrice(expr)
    case Product(name, brand, quantity) =>
      val b = brand.getOrElse(productSvc.getDefaultBrand(name))
      productSvc.getPrice(name, b) * quantity
    case _ => 0.0
  end computePrice

  /** Prepare the command by making the products. Each product of the same type
    * is made one by one. For different types, the products are made in
    * parallel.
    *
    * @param t
    *   : the products to prepare
    * @return
    *   a future that will be completed when the products are ready
    */
  def prepareCommand(t: ExprTree): Future[ExprTree] = t match
    case Product(name, brand, quantity) =>
      val b = brand.getOrElse(productSvc.getDefaultBrand(name))

      def prepareInSerial(remaining: Int, acc: Int): Future[Int] = {
        if remaining == 0 then Future.successful(acc)
        else
          val future = productSvc.prepare(name, b)
          future.transformWith {
            case Success(_) => prepareInSerial(remaining - 1, acc + 1)
            case Failure(_) => prepareInSerial(remaining - 1, acc)
          }
      }

      prepareInSerial(quantity, 0).flatMap(madeQuantity =>
        madeQuantity match
          case 0 => Future.failed(null)
          case _ => Future.successful(Product(name, brand, madeQuantity))
      )
    case And(left, right) =>
      // Source: https://stackoverflow.com/a/20874404/9188650
      val futures = List(prepareCommand(left), prepareCommand(right))
        .map(_.transform(Success(_)))
      val seq = Future.sequence(futures)
      val successes = seq.map(_.collect { case Success(x) => x })
      successes.flatMap(l =>
        l.size match {
          case 0 => Future.failed(null)
          case 1 => Future.successful(l.head)
          case 2 => Future.successful(And(l.head, l.last))
        }
      )
    case Or(left, right) =>
      if computePrice(left) <= computePrice(right) then prepareCommand(left)
      else prepareCommand(right)
    case _ => Future.successful(t)

  end prepareCommand

  /** Verify if the current node requires user to be logged.
    *
    * @param t
    *   : the current node
    * @return
    *   true if the user needs to be logged, false otherwise
    */
  def requiresLogging(t: ExprTree): Boolean = t match
    case Command(_) | Balance => true
    case _                    => false
  end requiresLogging

  /** Return the output text of the current node, in order to write it in
    * console.
    * @return
    *   A tuple containing:
    * -- the output text of the current node
    * -- an optional future that will be completed when the products are ready
    */
  def reply(session: Session)(t: ExprTree): (String, Option[Future[String]]) =
    if requiresLogging(t) && session.getCurrentUser.isEmpty then
      return (
        "Vous devez vous identifier pour pouvoir faire cette action !",
        None
      )

    val inner: ExprTree => (String, Option[Future[String]]) = reply(session)
    val user = session.getCurrentUser
    t match
      case Thirsty =>
        (
          "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !",
          None
        )
      case Hungry =>
        (
          "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !",
          None
        )
      case Identification(username) =>
        session.setCurrentUser(username)
        if !accountSvc.isAccountExisting(username) then
          accountSvc.addAccount(username)
        ("Bienvenue " + username + " !", None)
      case Balance =>
        (
          s"Le montant actuel de votre solde est de CHF ${accountSvc
              .getAccountBalance(user.get)}",
          None
        )
      case Price(expr) =>
        (s"Cela coûte CHF ${computePrice(expr)} !", None)
      case command @ Command(expr) =>
        val price = computePrice(command)
        if price > accountSvc.getAccountBalance(user.get) then
          return (TOO_POOR, None)

        val answer = prepareCommand(expr)
          .map(t => {
            val toPay = computePrice(t)
            try {
              accountSvc.purchase(user.get, price)
              if toPay == price then
                s"La commande de ${inner(expr)._1} est prête. Cela coute $toPay.-"
              else
                s"La commande de ${inner(expr)._1} est partiellement prête. Voici ${inner(t)._1}. Cela coute $toPay.-"
            } catch {
              case e: Exception => TOO_POOR
            }
          })
          .recover(_ =>
            s"La commande de ${inner(expr)._1} ne peut pas être délivrée."
          )

        (
          s"Votre commande est en cours de préparation: ${inner(expr)._1}",
          Some(answer)
        )

      case Product(name, brand, quantity) =>
        (productSvc.toString(name, brand, quantity), None)

      case And(left, right) =>
        (inner(left)._1 + " et " + inner(right)._1, None)
      case Or(left, right) =>
        (
          if computePrice(left) <= computePrice(right) then s"${inner(left)}"
          else s"${inner(right)}",
          None
        )
  end reply

end AnalyzerService
