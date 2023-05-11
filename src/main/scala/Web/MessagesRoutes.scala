package Web

import Chat.{AnalyzerService, TokenizerService}
import Data.{MessageService, AccountService, SessionService, Session}
import scala.collection.mutable.ListBuffer
import cask.endpoints.WsChannelActor
import castor.Context.Simple.global

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

  private def latestMessagesAsString(number: Int) =
    if msgSvc.getLatestMessages(number).isEmpty then
      Layouts.placeholderElem("No messages have been sent yet").toString
    else
      msgSvc
        .getLatestMessages(number)
        .reverse
        .map((author, content) => Layouts.messageElem(author, content).toString)
        .reduceLeft(_ + _)

  private def sendMessageToClient(
      channel: cask.endpoints.WsChannelActor,
      message: String
  ) =
    channel.send(cask.Ws.Text(message))

  @getSession(sessionSvc)
  @cask.get("/")
  def index()(session: Session) =
    Layouts.indexPage(session)

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
      msgSvc.add(
        session.getCurrentUser.get,
        Layouts.messageContent(msg)
      )
      // Send the last 20 messages to all the subscribers
      val messages = latestMessagesAsString(20)
      subscribers.foreach(sendMessageToClient(_, messages))

      response(true, None)
    end if
  end sendMessage
  //
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
  //
  // TODO - Part 3 Step 4d: Delete the message history when a GET is made to `/clearHistory`
  //
  // TODO - Part 3 Step 5: Modify the code of step 4b to process the messages sent to the bot (message
  //      starts with `@bot `). This message and its reply from the bot will be added to the message
  //      store together.
  //
  //      The exceptions raised by the `Parser` will be treated as an error (same as in step 4b)

  initialize()
end MessagesRoutes
