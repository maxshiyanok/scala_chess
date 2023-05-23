package chess

import java.io.File
import java.util.concurrent.Executors

import cats.effect.concurrent.Ref
import cats.effect.{Blocker, ContextShift, Sync}
import fs2.concurrent.{Queue, Topic}
import fs2.{Pipe, Stream}
import org.http4s.dsl.Http4sDsl
import org.http4s.headers.`Content-Type`
import org.http4s.server.websocket.WebSocketBuilder
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.{Close, Text}
import org.http4s.{HttpRoutes, MediaType, StaticFile}


class ChessRoutes[F[_]: Sync: ContextShift](
    chessState: Ref[F, ChessState],
    queue: Queue[F, InputMessage],
    topic: Topic[F, OutputMessage]
) extends Http4sDsl[F] {

  private val blocker = {
    val numBlockingThreadsForFilesystem = 4
    val blockingPool                    = Executors.newFixedThreadPool(numBlockingThreadsForFilesystem)
    Blocker.liftExecutorService(blockingPool)
  }

  val routes: HttpRoutes[F] =
    HttpRoutes.of[F] {
      // Static resources
      case request @ GET -> Root =>
        StaticFile
          .fromFile(new File("static/index.html"), blocker, Some(request))
          .getOrElseF(NotFound())
        
      case request @ GET -> Root / "chat.js" =>
        StaticFile
          .fromFile(new File("static/chat.js"), blocker, Some(request))
          .getOrElseF(NotFound())

      case GET -> Root / "ws" / userName =>
        val toClient: Stream[F, WebSocketFrame.Text] =
          topic
            .subscribe(1000)
            .filter(_.forUser(User(userName)))
            .map(msg => Text(msg.toString))

        def processInput(wsfStream: Stream[F, WebSocketFrame]): Stream[F, Unit] = {

          val entryStream: Stream[F, InputMessage] = Stream.emits(Seq(EnterRoom(User(userName), InputMessage.defaultLobbyName)))

          val parsedWebSocketInput: Stream[F, InputMessage] =
            wsfStream
              .collect {
                case Text(text, _) => InputMessage.parse(User(userName), text)

                case Close(_) => Disconnect(User(userName))
              }

          (entryStream ++ parsedWebSocketInput).through(queue.enqueue)
        }

        val inputPipe: Pipe[F, WebSocketFrame, Unit] = processInput

        WebSocketBuilder[F].build(toClient, inputPipe)
    }
}
