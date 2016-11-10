package smtp

import fastparse.core.Parsed.Success
import org.scalatest.{FlatSpec, MustMatchers}

class ParserSpec extends FlatSpec with MustMatchers {


  "Greeting" must "parse a greeting" in {
    val input = "HELO nicememe.website.website.website.website.website.website"
    val result = smtp.Parser.Greeting.parse(input)
    result must equal(Success(AST.Greeting("nicememe.website.website.website.website.website.website"), input.size))
  }


  "MailFrom" must "parse mailfrom messages" in {
    val input = "MAIL FROM:<bob@example.org>"
    val result = smtp.Parser.MailFrom.parse(input)
    result must equal(Success(AST.MailFrom(AST.Email("bob", "example.org")), input.size))
  }

  "ReceiptTo" must "parse receipt to messages" in {
    val input = "RCPT TO:<boss@somewhere.org>"
    val result = smtp.Parser.ReceiptTo.parse(input)
    result must equal(Success(AST.ReceiptTo(AST.Email("boss", "somewhere.org")), input.size))
  }

  "MailConfiguration" must "parse mailfrom messages" in {
    val input = "MAIL FROM:<bob@example.org>"
    val result = smtp.Parser.MailConfiguration.parse(input)
    result must equal(Success(AST.MailFrom(AST.Email("bob", "example.org")), input.size))
    val input1 = "RCPT TO:<boss@somewhere.org>"
    val result1 = smtp.Parser.MailConfiguration.parse(input1)
    result1 must equal(Success(AST.ReceiptTo(AST.Email("boss", "somewhere.org")), input1.size))
  }

  "CompleteEmail" must "parse a complete email" in {
    val email = """To: "Bob" <bob@eve>
      |To: "Terry Lastname" <terry lastname@eve>
      |Subject: cool evemail
      |
      |Body goes here
      |Yup
      |it's a multiline body
    """.stripMargin
    val res = Parser.completeEmail.parse(email)
    println(res)
  }


}
