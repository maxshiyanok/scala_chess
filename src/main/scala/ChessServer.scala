package chess

import cats.effect._
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.Stream
import fs2.concurrent.{Queue, Topic}
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.syntax.kleisli._

import scala.concurrent.duration._
import scala.util.Try


object ChessServer extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val httpPort = 8080

    for (
         queue <- Queue.unbounded[IO, InputMessage];
         topic <- Topic[IO, OutputMessage](SendToUsers(Set.empty, ""));

         ref <- Ref.of[IO, ChessState](ChessState());


         exitCode <- {
           val httpStream = ServerStream.stream[IO](httpPort, ref, queue, topic)

           val keepAlive = Stream.awakeEvery[IO](30.seconds).map(_ => KeepAlive).through(topic.publish)

           val processingStream =
             queue.dequeue
               .evalMap(msg => ref.modify(_.process(msg)))
               .flatMap(Stream.emits)
               .through(topic.publish)

           Stream(httpStream, keepAlive, processingStream).parJoinUnbounded.compile.drain
             .as(ExitCode.Success)
         }) yield exitCode
  }
}

object ServerStream {

  def stream[F[_]: ConcurrentEffect: Timer: ContextShift](
      port: Int,
      chessState: Ref[F, ChessState],
      queue: Queue[F, InputMessage],
      topic: Topic[F, OutputMessage]
  ): fs2.Stream[F, ExitCode] =
    BlazeServerBuilder[F]
      .bindHttp(port, "0.0.0.0")
      .withHttpApp(
        Router(
          "/" -> new ChessRoutes[F](chessState, queue, topic).routes
        ).orNotFound
      )
      .serve
}
