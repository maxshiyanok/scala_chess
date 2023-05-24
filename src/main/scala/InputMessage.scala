package chess

sealed trait InputMessage {
  val user: User
}

case class Play(user: User, userInput: String)         extends InputMessage
case class EnterRoom(user: User, room: String)    extends InputMessage
case class ListRooms(user: User)                  extends InputMessage
case class ListMembers(user: User)                extends InputMessage
case class Disconnect(user: User)                 extends InputMessage
case class InvalidInput(user: User, userInput: String) extends InputMessage

object InputMessage {
  val defaultLobbyName = "defaultLobbyName"

  def parse(user: User, userInput: String): InputMessage =
    splitFirstTwoWords(userInput) match {
      case ("/room", "", "")   => EnterRoom(user, defaultLobbyName)
      case ("/room", room, "") => EnterRoom(user, room)
      case ("/room", _, _)     => InvalidInput(user, "/room takes a single, optional argument")
      case ("/rooms", _, _)    => ListRooms(user)
      case ("/members", _, _)  => ListMembers(user)
      case (s"/$cmd", _, _)    => InvalidInput(user, s"unknown command - $cmd")
      case _                   => Play(user, userInput)
    }

  private def splitFirstWord(userInput: String): (String, String) = {
    val trimmedText = userInput.trim
    val firstSpace  = trimmedText.indexOf(' ')
    if (firstSpace < 0)
      (trimmedText, "")
    else
      (trimmedText.substring(0, firstSpace), trimmedText.substring(firstSpace + 1).trim)
  }

  private def splitFirstTwoWords(userInput: String): (String, String, String) = {
    val (first, intermediate) = splitFirstWord(userInput)
    val (second, rest)        = splitFirstWord(intermediate)

    (first, second, rest)
  }
}
