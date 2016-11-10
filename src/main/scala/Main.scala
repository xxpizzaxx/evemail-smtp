import java.net.InetSocketAddress
import java.nio.channels.AsynchronousChannelGroup
import java.util.concurrent.Executors

import scalaz.stream.Cause._
import Process._
import java.net.InetSocketAddress
import java.nio.charset.Charset
import java.util.concurrent.ScheduledExecutorService

import fastparse.core.Parsed
import fastparse.core.Parsed.{Failure, Success}

import scala.concurrent.SyncVar
import scala.util.Random
import scalaz.{-\/, State, \/, \/-}
import scalaz.concurrent.Task
import scalaz.stream.Process.Halt
import scalaz.stream.ReceiveY._
import scodec.bits.ByteVector

import scalaz.stream._
import scodec._
import smtp.{AST, StatusCodes}

import scalaz.stream.{Writer1, merge, nio}

object Main {
  implicit val S = scalaz.concurrent.Strategy.DefaultStrategy
  implicit val AG = nio.DefaultAsynchronousChannelGroup
  import tcp.syntax._

  import scala.concurrent.duration._

  implicit class OptionableParsed[+T, Elem, Repr](p: Parsed[T, Elem, Repr]) {
    def toOption: Option[T] = {
      p match {
        case Success(t, _) => Option(t)
        case Failure(_, _, _) => Option.empty[T]
      }
    }
  }

  case class SMTPServerState(hasGreetedMe: Boolean = false,
                             from: List[AST.Email] = List.empty[AST.Email],
                             receipt: List[AST.Email] = List.empty[AST.Email],
                             datamode: Boolean = false)

  def doAction(s: SMTPServerState, line: String): (SMTPServerState, String) = {
    val command = line.trim
    s match {
      case _ if !s.hasGreetedMe =>
        // if we haven't been greeted, let's see if we're being greeted
        smtp.Parser.Greeting.parse(command) match {
          case Success(greeting, _) =>
            (s.copy(hasGreetedMe = true), StatusCodes.MailActionOkay.render)
          case Failure(_, _, _) => (s, StatusCodes.ServiceReady.render)
        }
      case _ if !s.datamode =>
        // we've been greeted, we're in the main loop
        // check to see if it's a from or receipt to
        smtp.Parser.MailConfiguration
          .parse(command)
          .toOption
          .map {
            _ match {
              case f: AST.MailFrom =>
                (s.copy(from = s.from :+ f.from),
                 StatusCodes.MailActionOkay.render)
              case r: AST.ReceiptTo =>
                (s.copy(receipt = s.receipt :+ r.from),
                 StatusCodes.MailActionOkay.render)
            }
          }
          .map(Option(_))
          .getOrElse {
            smtp.Parser.DATA.parse(command).toOption.map { _ =>
              (s.copy(datamode = true), StatusCodes.GiveData.render)
            }
          } getOrElse {
          (s, StatusCodes.SyntaxError.render)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val address = new InetSocketAddress(25)
    tcp
      .server(address, concurrentRequests = 10) {
        tcp.writes_ {
          (Process.emit("\n") ++ (tcp.reads(1024).pipe(text.utf8Decode)))
            .stateScan(SMTPServerState()) { line =>
              for {
                state <- State.get[SMTPServerState]
                _ = println(state)
                (s, m) = doAction(state, line)
                _ <- State.put(s)
              } yield m
            }
            .map(_ + "\r\n")
            .pipe(text.utf8Encode)
        }
      }
      .map(_.run.run)
      .run
      .run
  }

}
