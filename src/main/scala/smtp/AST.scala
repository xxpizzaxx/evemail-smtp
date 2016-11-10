package smtp

import org.http4s.dsl.Created

object AST {
  case class StatusCode(code: Int, message: String) {
    def render: String = s"$code $message"
  }

  case class Email(username: String, domain: String)
  case class EmailWithName(name: String, email: Email)


  // expression ASTs
  case class Greeting(fqdn: String)

  trait MailConfiguration
  case class MailFrom(from: Email) extends MailConfiguration
  case class ReceiptTo(from: Email) extends MailConfiguration







}
