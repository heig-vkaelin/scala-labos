package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import Data.Session

object Layouts:
  /** Link the CSS and JS files to the HTML page.
    *
    * @return
    *   the head element of the HTML page
    */
  private def headElem() = head(
    script(src := "/static/js/main.js"),
    tag("link")(href := "/static/css/main.css", rel := "stylesheet")
  )

  /** Display an element from the main navigation
    * @param text
    *   the text to display
    * @param linkHref
    *   : the desired link
    * @return
    *   the nav element
    */
  private def navElem(
      text: String,
      linkHref: String,
      additionnalText: Option[String] = None
  ) =
    tag("nav")(
      a(cls := "nav-brand")("Bot-Tender"),
      div(additionnalText),
      div(
        cls := "nav-item"
      )(
        a(href := linkHref)(text)
      )
    )

    /** Display the form to send a message
      *
      * @param idForm
      *   the HTML id of the form
      * @param onSubmit
      *   the action to do when the form is submitted
      * @param label
      *   the label text of the input
      * @param errorId
      *   the HTML id of the error div
      * @param errorText
      *   the error text to display
      * @param inputId
      *   the HTML id of the input
      * @return
      *   the form element
      */
  private def formElem(
      idForm: String,
      onSubmit: Option[String],
      actionForm: Option[String],
      methodForm: Option[String],
      label: String,
      errorId: String,
      errorText: Option[String],
      inputId: String,
      btnText: String
  ) = form(
    id := idForm,
    onsubmit := onSubmit.getOrElse(""),
    action := actionForm.getOrElse(""),
    method := methodForm.getOrElse("")
  )(
    div(
      id := errorId,
      cls := "errorMsg",
      if errorText.isEmpty then display := "none"
    )(errorText.getOrElse("")),
    tag("label")(attr("for") := inputId)(label),
    input(id := inputId, name := inputId, tpe := "text"),
    input(tpe := "submit", value := btnText)
  )

  /** Display a message (not used for now)
    *
    * @param author
    *   the author name
    * @param mention
    *   the mention name
    * @param content
    *   the content of the message
    * @return
    *   the message element
    */
  private def messageElem(author: String, mention: String, content: String) =
    div(
      cls := "msg"
    )(
      span(
        cls := "author"
      )(author),
      span(
        cls := "msg-content"
      )(
        span(
          cls := "mention"
        )(mention),
        raw(content)
      )
    )

  /** Display the index page of the application
    *
    * @param session
    *   the current session
    * @return
    *   the HTML page
    */
  def indexPage(session: Session) =
    html(
      headElem(),
      body(
        session.getCurrentUser
          .map(username =>
            navElem("Logout", "/logout", Some(s"Hello $username!"))
          )
          .getOrElse(navElem("Login", "/login")),
        div(
          cls := "content"
        )(
          div(
            id := "boardMessage"
          )(
            div(cls := "msg", textAlign.center)(
              "Please wait! The messages are loading..."
            )
          ),
          formElem(
            "msgForm",
            Some("submitMessageForm(); return false;"),
            None,
            None,
            "Your message:",
            "errorDiv",
            None,
            "messageInput",
            "Envoyer"
          )
        )
      )
    )

    /** Display the login and register page of the application
      *
      * @param loginError
      *   the error message to display for the login form
      * @param registerError
      *   the error message to display for the register form
      * @return
      *   the HTML page
      */
  def loginPage(loginError: Option[String], registerError: Option[String]) =
    html(
      headElem(),
      body(
        navElem("Go to the message board", "/"),
        div(
          cls := "content"
        )(
          h1("Login"),
          formElem(
            "loginForm",
            None,
            Some("/login"),
            Some("post"),
            "Username:",
            "errorLogin",
            loginError,
            "loginInput",
            "Envoyer"
          ),
          h1("Register"),
          formElem(
            "registerForm",
            None,
            Some("/register"),
            Some("post"),
            "Username:",
            "errorRegister",
            registerError,
            "registerInput",
            "Envoyer"
          )
        )
      )
    )

  /** Display the success page of the application
    *
    * @param message
    *   the message to display
    * @return
    *   the HTML page
    */
  def successPage(message: String) =
    html(
      headElem(),
      body(
        navElem("Go to the message board", "/"),
        div(
          cls := "content"
        )(
          h1("Success!"),
          div(message)
        )
      )
    )

end Layouts
