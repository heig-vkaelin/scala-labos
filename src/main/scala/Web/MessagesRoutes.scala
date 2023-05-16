package Web

import Chat.{AnalyzerService, TokenizerService}
import Data.{MessageService, AccountService, SessionService, Session}
import scala.collection.mutable.ListBuffer
import cask.endpoints.WsChannelActor
import castor.Context.Simple.global
import Chat.{Parser, UnexpectedTokenException}
import Chat.ExprTree.Identification
import Chat.ExprTree

/** Assembles the routes dealing with the message board:
  *   - One route to display the home page
  *   - One route to send the new messages as JSON
  *   - One route to subscribe with websocket to new messages
  *
  * @param log
  */
class MessagesRoutes(
    tokenizerSvc: TokenizerService,
    analyzerSvc: AnalyzerService,
    msgSvc: MessageService,
    accountSvc: AccountService,
    sessionSvc: SessionService
)(implicit val log: cask.Logger)
    extends cask.Routes:
  import Decorators.getSession

  val subscribers = ListBuffer[WsChannelActor]()

  private def response(success: Boolean, error: Option[String]) =
    ujson.Obj("success" -> success, "err" -> error.getOrElse(""))
  end response

  private def latestMessagesAsString(number: Int) =
    if msgSvc.getLatestMessages(number).isEmpty then
      Layouts.placeholderElem("No messages have been sent yet").toString
    else
      msgSvc
        .getLatestMessages(number)
        .reverse
        .map((author, content) => Layouts.messageElem(author, content).toString)
        .reduceLeft(_ + _)
  end latestMessagesAsString

  private def sendMessageToClient(
      channel: cask.endpoints.WsChannelActor,
      message: String
  ) =
    channel.send(cask.Ws.Text(message))
  end sendMessageToClient

  private def getMention(msg: String): Option[String] =
    if msg.charAt(0) == '@' then Some(msg.substring(0, msg.indexOf(" ")))
    else None
  end getMention

  private def botResponse(
      message: String,
      mention: Option[String],
      expr: Option[ExprTree],
      botMessage: String
  )(session: Session) =
    val replyId = msgSvc.add(
      session.getCurrentUser.get,
      Layouts.messageContent(message, mention),
      mention,
      expr
    )
    msgSvc.add(
      "BotTender",
      Layouts.messageContent(botMessage),
      session.getCurrentUser,
      None,
      Some(replyId)
    )
  end botResponse

  private def processMessage(message: String)(
      session: Session
  ): Option[String] =
    val mention = getMention(message)

    if mention.isDefined && mention.get == "@bot" then
      val content = message.substring(message.indexOf(" "))
      val tokenized = tokenizerSvc.tokenize(content)
      try {
        val expr = new Parser(tokenized).parsePhrases()
        val answer = expr match
          case Identification(username) => s"Bonjour $username"
          case _                        => analyzerSvc.reply(session)(expr)

        botResponse(content, mention, Some(expr), answer)(session)
      } catch {
        case e: Chat.UnexpectedTokenException =>
          return Some(e.getMessage)
      }
    else
      msgSvc.add(
        session.getCurrentUser.get,
        Layouts.messageContent(message),
        mention
      )

    val messages = latestMessagesAsString(20)
    subscribers.foreach(sendMessageToClient(_, messages))
    None
  end processMessage

  @getSession(sessionSvc)
  @cask.get("/")
  def index()(session: Session) =
    Layouts.indexPage(session)
  end index

  // TODO - Part 3 Step 4b:
  // Process the new messages sent as JSON object to `/send`. The JSON looks
  //      like this: `{ "msg" : "The content of the message" }`.
  //
  //      A JSON object is returned. If an error occurred, it looks like this:
  //      `{ "success" : false, "err" : "An error message that will be displayed" }`.
  //      Otherwise (no error), it looks like this:
  //      `{ "success" : true, "err" : "" }`
  //
  //      The following are treated as error:
  //      - No user is logged in
  //      - The message is empty
  //
  //      If no error occurred, every other user is notified with the last 20 messages
  @getSession(sessionSvc)
  @cask.postJson("/send")
  def sendMessage(msg: String)(session: Session) =
    if msg.isEmpty then response(false, Some("A message can't be empty"))
    else if session.getCurrentUser.isEmpty then
      response(false, Some("You must be logged to send a message"))
    else
      val error = processMessage(msg)(session)
      response(error.isEmpty, error)
    end if
  end sendMessage

  // TODO - Part 3 Step 4c: Process and store the new websocket connection made to `/subscribe`
  @cask.websocket("/subscribe")
  def subscribe(): cask.WebsocketResult =
    cask.WsHandler { channel =>
      subscribers += channel

      sendMessageToClient(channel, latestMessagesAsString(20))

      cask.WsActor { case cask.Ws.Close(_, _) =>
        subscribers -= channel
      }
    }
  end subscribe

  // TODO - Part 3 Step 4d: Delete the message history when a GET is made to `/clearHistory`
  @cask.get("/clearHistory")
  def clearHistory() =
    msgSvc.deleteHistory()
    val placeholder =
      Layouts.placeholderElem("No messages have been sent yet").toString
    subscribers.foreach(sendMessageToClient(_, placeholder))
  end clearHistory

  // TODO - Part 3 Step 5: Modify the code of step 4b to process the messages sent to the bot (message
  //      starts with `@bot `). This message and its reply from the bot will be added to the message
  //      store together.
  //
  //      The exceptions raised by the `Parser` will be treated as an error (same as in step 4b)

  initialize()
end MessagesRoutes
