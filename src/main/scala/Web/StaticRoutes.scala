package Web

/** Assembles the routes dealing with static files.
  */
class StaticRoutes()(implicit val log: cask.Logger) extends cask.Routes:
  @cask.staticResources("/static/js")
  def staticJSFiles() = "./js"
  @cask.staticResources("/static/css")
  def staticCSSFiles() = "./css"

  initialize()
end StaticRoutes
