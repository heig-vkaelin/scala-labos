# SCALA - Labo Bot-Tender: Future

**Auteurs: Alexandre Jaquier, Stéphane Marengo et Valentin Kaelin**

## Choix architecturaux et d'implémentation

Pour commencer, nous n'avons pas créé de services ou de classes externes suplémentaires. En effet, le code à ajouter pour gérer la notion de future est assez minime et nous avons préféré modifier les classes existantes aux endroits nécessaires.

La discussion qui suit est organisée par fichier modifié.

### ProductService.scala

Nous avons créé une case class `Delivery` qui contient les informations de préparation d'un produit:

```scala
object ProductService:
  // ...
  case class Delivery(mean: Duration, std: Duration, successRate: Double)
```

Cette classe est utilisée dans la map des produits, qui contient donc ces informations en plus des prix:

```scala
private val products = Map(
    BEER -> Map(
      "boxer" -> (1.0, Delivery(1.second, 0.2.second, 0.9)),
      "farmer" -> (1.0, Delivery(0.5.second, 0.2.second, 0.7)),
      // ...
    ),
    CROISSANT -> Map(
      "maison" -> (2.0, Delivery(2.second, 0.2.second, 0.9)),
      "cailler" -> (2.0, Delivery(3.5.second, 0.2.second, 0.7))
    )
  )
```

Une méthode simulant la préparation d'un produit `prepare` a été ajoutée, elle utilise la méthode `randomSchedule` fournie:

```scala
def prepare(product: ProductName, brand: BrandName): Future[Unit] =
    val (_, delivery) = products(product)(brand)
    randomSchedule(delivery.mean, delivery.std, delivery.successRate)
```

### AccountService.scala

La seule modification apportée dans cette classe est la gestion de la concurrence lors de l'achat d'un produit. La méthode `updateWith` permet de rendre l'opération atomique dans la méthode `purchase`.

```scala
def purchase(user: String, amount: Double): Double =
    accounts
      .updateWith(user)(current =>
        if current.get >= amount then Some(current.get - amount)
        else throw new Exception("Not enough money")
      )
      .get
```

### AnalyzerService.scala

La principale partie de la logique de ce laboratoire se trouve dans la classe `AnalyzerService`.

**prepareCommand**

Tout d'abord, une méthode `prepareCommand` a été créée afin de gérer la création de futures pour les commandes. Cette méthode est appelée dans la méthode `reply` dont nous discuterons après. Les informations intéressantes à propos du code ont été ajoutées sous forme de commentaire ci-dessous.

```scala
def prepareCommand(t: ExprTree): Future[ExprTree] = t match
    case Product(name, brand, quantity) =>
      val b = brand.getOrElse(productSvc.getDefaultBrand(name))

      // Lors de la prépation d'une quantité d'un produit, une méthode récursive a été créée
      // afin de préparer les produits en série, l'un après l'autre.
      def prepareInSerial(remaining: Int, acc: Int): Future[Int] = {
        if remaining == 0 then Future.successful(acc)
        else
          val future = productSvc.prepare(name, b)
          future.transformWith {
            case Success(_) => prepareInSerial(remaining - 1, acc + 1)
            case Failure(_) => prepareInSerial(remaining - 1, acc)
          }
      }

      // Une fois la quantité du produit souhaitée préparée nous vérifions le nombre
      // qui a été produit en réalité. Tant qu'il n'est pas nul, l'opération est réussie
      prepareInSerial(quantity, 0).flatMap(madeQuantity =>
        madeQuantity match
          case 0 => Future.failed(null)
          case _ => Future.successful(Product(name, brand, madeQuantity))
      )
    case And(left, right) =>
      // Pour gérer la commande de plusieurs types de produits, nous lançons les préparations
      // en parallèle en créant une séquence de futures. Nous empêchons l'échec de l'entièreté des futures
      // dans le cas où l'un de ceux-ci échoue en les transformants en Future de Try
      val futures = List(prepareCommand(left), prepareCommand(right))
        .map(_.transform(Success(_)))
      val seq = Future.sequence(futures)

      // Nous gardons tous les produits qui ont bien été préparés et
      // vérifions que la commande contient au moins un produit bien préparé,
      // dans le cas contraire elle est considérée comme échouée.
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
```

**Type de retour de reply**

La signature de la méthode `reply` a été modifiée afin de pouvoir potentiellement retourner un `Future` contenant le message de fin de préparation des produits. Ce `Future` est retourné comme second paramètre optionnel.

```scala
def reply(session: Session)(t: ExprTree): (String, Option[Future[String]])
```

**Vérification du solde dans reply**

Lors d'une commande, nous devons vérifier qu'à la fin de la préparation de la commande, l'utilisateur ait encore un solde suffisant pour payer la commande (il aurait pu dépenser son argent entre temps via une autre commande plus rapide par exemple). Cela est fait dans la méthode `purchase` vue précédemment qui lance une exception dans ce cas.

Si le `Future` retourné par `prepareCommand` est un échec, nous retournons dans le `Future` un message expliquant que la commande n'a pas pu être délivrée.

```scala
def reply(session: Session)(t: ExprTree): (String, Option[Future[String]]) =
  // ...
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
          // Si le solde est insuffisant, nous retournons un message d'erreur
          case e: Exception => TOO_POOR
        }
      })
      // Si aucun produit n'a pu être préparé
      .recover(_ =>
        s"La commande de ${inner(expr)._1} ne peut pas être délivrée."
      )

    (
      s"Votre commande est en cours de préparation: ${inner(expr)._1}",
      Some(answer)
    )
    // ...
  end reply
```

### MessagesRoutes.scala

Pour finir, dans les routes de l'application web, la méthode `processMessage` qui s'occupe de gérer le message envoyé par l'utilisateur a été modifiée.

```scala
private def processMessage(message: String)(
      session: Session
  ): Option[String] =
    // ...
      try {
        val expr = new Parser(tokenized).parsePhrases()
        val (answer, futureAnswer) = expr match
          case Identification(username) => (s"Bonjour $username", None)
          case _                        => analyzerSvc.reply(session)(expr)

        val idReply = botResponse(content, mention, Some(expr), answer)(session)
        if futureAnswer.isDefined then
          futureAnswer.get.map(content =>
            msgSvc.add(
              BOT_NAME,
              Layouts.messageContent(" " + content, session.getCurrentUser),
              session.getCurrentUser,
              None,
              Some(idReply)
            )
            updateDisplay()
          )
      // ...
  end processMessage
```

Si la méthode modifiée précédemment `reply` retourne un `Future` comme deuxième paramètre, le bot crée et ajoute le message de réponse une fois le `Future` terminé. L'affichages de tous les utilisateurs est ensuite mis à jour pour refléter le nouveau message.
