package Web

import Data.{AccountService, SessionService, Session}

/** Assembles the routes dealing with the users:
  *   - One route to display the login form and register form page
  *   - One route to process the login form and display the login success page
  *   - One route to process the register form and display the register success
  *     page
  *   - One route to logout and display the logout success page
  *
  * The username of the current session user is stored inside a cookie called
  * `username`.
  */
class UsersRoutes(accountSvc: AccountService, sessionSvc: SessionService)(
    implicit val log: cask.Logger
) extends cask.Routes:
  import Decorators.getSession

  /** Display the login form and register form page
    *
    * @param session
    *   the current session
    * @return
    *   the HTML page
    */
  @getSession(sessionSvc)
  @cask.get("/login")
  def login()(session: Session) =
    Layouts.loginPage(None, None)
  end login

  /** Process the login information sent by the login form
    *
    * @param loginInput
    *   the username sent by the login form
    * @param session
    *   the current session
    * @return
    *   the login page with error message or a success page
    */
  @getSession(sessionSvc)
  @cask.postForm("/login")
  def loginPost(loginInput: String)(session: Session) =
    if accountSvc.isAccountExisting(loginInput) then
      session.setCurrentUser(loginInput)
      Layouts.successPage("You are now logged in!")
    else Layouts.loginPage(Some("The specified user does not exist"), None)
  end loginPost

  /** Process the register information sent by the register form
    *
    * @param registerInput
    *   the username sent by the register form
    * @param session
    *   the current session
    * @return
    *   the register page with error message or a success page
    */
  @getSession(sessionSvc)
  @cask.postForm("/register")
  def registerPost(registerInput: String)(session: Session) =
    if registerInput.isEmpty() then
      Layouts.loginPage(None, Some("The specified user is invalid"))
    else if accountSvc.isAccountExisting(registerInput) then
      Layouts.loginPage(None, Some("The specified user already exists"))
    else
      accountSvc.addAccount(registerInput)
      session.setCurrentUser(registerInput)
      Layouts.successPage("You have been registered!")
  end registerPost

  /** Reset the current session and display a successful logout page.
    *
    * @param session
    *   the current session
    * @return
    *   a success page
    */
  @getSession(sessionSvc)
  @cask.get("/logout")
  def logout()(session: Session) =
    session.reset()
    Layouts.successPage("You have been logged out!")
  end logout

  initialize()
end UsersRoutes
