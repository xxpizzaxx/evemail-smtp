package smtp

object StatusCodes {

  val ServiceReady = AST.StatusCode(220, "evemail.pizza.moe evemail relay server")
  val Closing = AST.StatusCode(221, "Bye")
  val MailActionOkay = AST.StatusCode(250, "Ok")
  val GiveData = AST.StatusCode(354, "End data with <CR><LF>.<CR><LF>")
  val SyntaxError = AST.StatusCode(500, "Syntax error, command unrecognised")

}
