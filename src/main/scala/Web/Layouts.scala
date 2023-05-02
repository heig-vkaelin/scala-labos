package Web

import scalatags.Text.all._
import scalatags.Text.tags2
import Data.Session

object Layouts:
  private def headElem() = head(
    script(src := "/static/js/main.js"),
    tag("link")(href := "/static/css/main.css", rel := "stylesheet")
  )

  private def navElem(leftText: String, linkHref: String) =
    tag("nav")(
      a(cls := "nav-brand")("Bot-Tender"),
      div(
        cls := "nav-item"
      )(
        a(href := linkHref)(leftText)
      )
    )

  private def formElem(
      idForm: String,
      onSubmit: Option[String],
      label: String,
      errorId: String,
      errorText: Option[String],
      inputId: String
  ) = form(
    id := idForm,
    onsubmit := onSubmit.getOrElse("")
  )(
    div(
      id := errorId,
      cls := "errorMsg",
      if errorText.isEmpty then display := "none"
    )(errorText.getOrElse("")),
    tag("label")(attr("for") := inputId)(label),
    input(id := inputId, tpe := "text"),
    input(tpe := "submit")
  )

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

  def indexPage(session: Session) =
    html(
      headElem(),
      body(
        session.getCurrentUser
          .map(_ => navElem("Logout", "/logout"))
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
            "Your message:",
            "errorDiv",
            None,
            "messageInput"
          )
        )
      )
    )

end Layouts
