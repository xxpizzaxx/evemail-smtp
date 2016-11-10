package smtp

object Parser {
  import fastparse.all._

  // keywords and primitives
  val HELO = P("HELO")
  val MAILFROM = P("MAIL FROM:")
  val RCPTTO = P("RCPT TO:")
  val DATA = P("DATA")
  val QUIT = P("QUIT")

  // characters
  val < = P("<")
  val > = P(">")
  val `@` = P("@")
  val lowercase = P(CharIn('a' to 'z'))
  val uppercase = P(CharIn('A' to 'Z'))
  val letter = P(lowercase | uppercase)
  val validUsernameSymbols = P("'" | " ")
  val validDomainSymbols = P(".")

  // usernames and domains
  val usernameCharacter = P(letter | validUsernameSymbols)
  val domainCharacter = P(letter | validDomainSymbols)
  val username = P(usernameCharacter.rep.!)
  val domain = P(domainCharacter.rep.!)

  // headers
 //val headerkey: P[String] = P(letter.rep.!)
  //val headervalue: P[String] = P()


  val emailnameWithQuotes: P[String] = P("\"" ~ username.rep.! ~ "\"")
  val fullEmailName: P[String] = P(emailnameWithQuotes | username)

  // expressions
  val Email: P[AST.Email] = P(< ~ username ~ `@` ~ domain ~ > ).map{case (u, d) => AST.Email(u, d)}
  val FullEmail: P[AST.EmailWithName] = P(fullEmailName ~ " " ~ Email).map{case (name, email) => AST.EmailWithName(name, email)}
  val Greeting: P[AST.Greeting] = P(HELO ~ " " ~ domain.!).map{fqdn => AST.Greeting(fqdn)}
  val MailFrom: P[AST.MailFrom] = P(MAILFROM ~/ Email).map(email => AST.MailFrom(email))
  val ReceiptTo: P[AST.ReceiptTo] = P(RCPTTO ~/ Email).map(email => AST.ReceiptTo(email))

  val MailConfiguration: P[AST.MailConfiguration] = P(MailFrom | ReceiptTo)

}
