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
  val BOT_NAME = "BotTender"
  val NB_LATEST_MESSAGES = 20

  /** Create a JSON response to inform if the request was successful or not
    *
    * @param success
    *   whether the request was successful
    * @param error
    *   an optional error message
    * @return
    *   a JSON object with the success status and an optional error message
    */
  private def response(success: Boolean, error: Option[String]) =
    ujson.Obj("success" -> success, "err" -> error.getOrElse(""))
  end response

  /** Get a string containing the latest messages
    *
    * @param number
    *   the number of messages to get
    * @return
    *   a string containing the latest messages
    */
  private def latestMessagesToString(number: Int) =
    if msgSvc.getLatestMessages(number).isEmpty then
      Layouts.placeholderElem("No messages have been sent yet").toString
    else
      msgSvc
        .getLatestMessages(number)
        .reverse
        .map((author, content) => Layouts.messageElem(author, content))
        .mkString
  end latestMessagesToString

  /** Send a message to one channel actor
    *
    * @param channel
    *   the channel actor to send the message to
    * @param message
    *   the message to send
    */
  private def sendMessageToClient(
      channel: WsChannelActor,
      message: String
  ) =
    channel.send(cask.Ws.Text(message))
  end sendMessageToClient

  /** Get an optional mention from a message
    * @param msg
    *   the message
    * @return
    *   an optional mention
    */
  private def getMention(msg: String): Option[String] =
    if msg.charAt(0) == '@' then Some(msg.substring(1, msg.indexOf(" ")))
    else None
  end getMention

  /** Create and store the messages from a bot response
    *
    * @param message
    *   the message from the user
    * @param mention
    *   the mention from the user
    * @param expr
    *   the type of request sent to the bot
    * @param botMessage
    *   the response from the bot
    * @param session
    *   the current session
    * @return
    *   the id of the message sent by the bot
    */
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
      BOT_NAME,
      Layouts.messageContent(botMessage),
      session.getCurrentUser,
      None,
      Some(replyId)
    )
  end botResponse

  /** Process a message sent by a user
    *
    * @param message
    *   the message sent by the user
    * @param session
    *   the current session
    * @return
    *   an optional error message
    */
  private def processMessage(message: String)(
      session: Session
  ): Option[String] =
    val mention = getMention(message)

    if mention.isDefined && mention.get == "bot" then
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

    val messages = latestMessagesToString(NB_LATEST_MESSAGES)
    subscribers.foreach(sendMessageToClient(_, messages))
    None
  end processMessage

  /** Display the home page
    *
    * @param session
    *   the current session
    * @return
    *   the HTML page
    */
  @getSession(sessionSvc)
  @cask.get("/")
  def index()(session: Session) =
    Layouts.indexPage(session)
  end index

  /** Process the new messages sent as JSON object to `/send` like this: `{"msg"
    * : "The content of the message" }`.
    *
    * A JSON object is returned. If an error occurred, it looks like this: `{
    * "success" : false, "err" : "An error message that will be displayed" }`.
    * Otherwise (no error), it looks like this: `{ "success" : true, "err" : ""
    * }`
    *
    * Messages can be sent to the bot (message starts with `@bot `). This
    * message and its reply from the bot will be added to the message store
    * together.
    *
    * The following are treated as error:
    *   - No user is logged in
    *   - The message is empty
    *   - The exceptions raised by the `Parser` when sending a message to the
    *     bot
    *
    * If no error occurred, every other user is notified with the last 20
    * messages
    *
    * @param msg
    *   the decoded message sent by the user
    * @param session
    *   the current session
    * @return
    *   a JSON object with the success status and an optional error
    */
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

  /** Process and store the new websocket connection made to `/subscribe` The
    * latest messages are sent to the new client
    *
    * @return
    *   the websocket handler
    */
  @cask.websocket("/subscribe")
  def subscribe(): cask.WebsocketResult =
    cask.WsHandler { channel =>
      subscribers += channel

      sendMessageToClient(channel, latestMessagesToString(NB_LATEST_MESSAGES))

      cask.WsActor { case cask.Ws.Close(_, _) =>
        subscribers -= channel
      }
    }
  end subscribe

  /** Delete the message history for all users
    *
    * @return
    *   a success page informing the history has been cleaned
    */
  @cask.get("/clearHistory")
  def clearHistory() =
    msgSvc.deleteHistory()
    val placeholder =
      Layouts.placeholderElem("No messages have been sent yet").toString
    subscribers.foreach(sendMessageToClient(_, placeholder))
    Layouts.successPage("The chat history has been cleaned!")
  end clearHistory

  initialize()
end MessagesRoutes
