# SCALA - Labo Bot-Tender: Future

**Auteurs: Alexandre Jaquier, Stéphane Marengo et Valentin Kaelin**

## Choix architecturaux et d'implémentation

<!-- A dire:

- ProductService.scala: ajouté la classe Delivery + méthode prépare
- AccountService.scala: modif la méthode purchase pour pouvoir modif dans un Future
- AnalyzerService.scala:
    - ajouté méthode prepareCommand qui gère tous les futures (parler serial vs parallel)
    - méthode reply retourne en plus un Future optionnel
    - méthode reply qui check aussi que le solde est tjr ok
- MessagesRoutes.scala: si reply contient un Futur, on l'affiche aussi quand il est finito

-->
