package Chat
import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import ExprTree._

  /** Compute the price of the current node, then returns it. If the node is not
    * a computational node, the method returns 0.0. For example if we had a "+"
    * node, we would add the values of its two children, then return the result.
    * @return
    *   the result of the computation
    */
  // TODO - Part 2 Step 3
  def computePrice(t: ExprTree): Double = t match
    case And(left, right) => computePrice(left) + computePrice(right)
    case Or(left, right)  => Math.min(computePrice(left), computePrice(right))
    case Command(expr)    => computePrice(expr)
    case Product(name, brand, quantity) =>
      val b = brand.getOrElse(productSvc.getDefaultBrand(name))
      productSvc.getPrice(name, b) * quantity
    case _ => 0.0

  def requiresLogging(session: Session, t: ExprTree): Boolean = t match
    case Command(_) | Balance => true
    case _                    => false

  /** Return the output text of the current node, in order to write it in
    * console.
    * @return
    *   the output text of the current node
    */
  def reply(session: Session)(t: ExprTree): String =
    if requiresLogging(session, t) && session.getCurrentUser.isEmpty then
      return "Vous devez vous identifier pour pouvoir faire cette action !"

    // you can use this to avoid having to pass the session when doing recursion
    val inner: ExprTree => String = reply(session)
    val user = session.getCurrentUser
    t match
      // TODO - Part 2 Step 3
      // Example cases
      case Thirsty =>
        "Eh bien, la chance est de votre côté, car nous offrons les meilleures bières de la région !"
      case Hungry =>
        "Pas de soucis, nous pouvons notamment vous offrir des croissants faits maisons !"
      case Identification(username) =>
        session.setCurrentUser(username)
        if !accountSvc.isAccountExisting(username) then
          accountSvc.addAccount(username)
        "Bienvenue " + username + " !"
      case Balance =>
        s"Le montant actuel de votre solde est de CHF ${accountSvc
            .getAccountBalance(user.get)}"
      case Price(expr) =>
        s"Cela coûte CHF ${computePrice(expr)} !"
      case command @ Command(expr) =>
        val price = computePrice(command)
        if price > accountSvc.getAccountBalance(user.get) then
          return "Vous n'avez pas assez d'argent pour effectuer cette commande ! Vous êtes pauvre (:"

        val newBalance = accountSvc.purchase(user.get, price)
        s"Voici donc ${inner(expr)} ! Cela coûte CHF ${price} et votre nouveau solde est de ${newBalance}"

      case Product(name, brand, quantity) =>
        productSvc.toString(name, brand, quantity)

      case And(left, right) =>
        inner(left) + " et " + inner(right)
      case Or(left, right) =>
        if computePrice(left) <= computePrice(right) then s"${inner(left)}"
        else s"${inner(right)}"

end AnalyzerService
