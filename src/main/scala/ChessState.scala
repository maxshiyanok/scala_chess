package chess

import scala.util.Random
import chess.side.White
import chess.side.Black

object ChessState {
  // Default constructor
  def apply(): ChessState = ChessState(Map.empty, Map.empty, Lobby(Set.empty))
}

case class User(
  name: String
)

case class Room(
  name: String,
  users: Vector[User],
  game: Option[Game]
)

case class Lobby(
  users: Set[User]
){
  def addToLobby(user: User): Lobby = {
    Lobby(users + user)
  }
  def removeFromLobby(user: User): Lobby = {
    Lobby(users - user)
  }
}

case class Game(
  playerWhite: User,
  playerBlack: User,
  gameState: GameState
)
object Game {
    def createGame(users: Vector[User]): Game = {
      val playersSideRandom = Random.between(0, 2)
      val playerWhite = users(playersSideRandom)
      val playerBlack = users(1-playersSideRandom)
      Game(playerWhite, playerBlack, GameState.createGame)
  }
}

case class ChessState(
    rooms: Map[String, Room],
    userRoom: Map[User, Room],
    lobby: Lobby
) {

  def process(msg: InputMessage): (ChessState, Seq[OutputMessage]) = msg match {

    case Play(user, text) =>
      userRoom.get(user) match {
        case Some(room) if !room.game.isEmpty =>
          updateGameInRoom(room, text, user)

        case _ =>
          (this, sendToLobby(s"${user.name}: $text"))
      }

    case EnterRoom(user, toRoom) =>
      toRoom match {
        case "defaultLobbyName" => {
          val (finalState, enterMessages) = addToLobby(user)
          (finalState, Seq(WelcomeUser(user)) ++ enterMessages)
        }
        case _ => userRoom.get(user) match {
          case None =>
            // First time in - welcome and enter
            addToRoom(user, toRoom)
          case Some(currentRoom: Room) if currentRoom.name == toRoom =>
            (this, Seq(SendToUser(user, "You are already in that room!")))

          case Some(_) =>
            // Already in - move from one room to another
            val (intermediateState, leaveMessages) = removeFromCurrentRoom(user)
            val (finalState, enterMessages)        = intermediateState.addToRoom(user, toRoom)

            (finalState, leaveMessages ++ enterMessages)
      }
      
      }

    case ListRooms(user) =>
      val roomList = rooms.keys.toList.sorted
        .mkString("Rooms:\n\t", "\n\t", "")

      (this, Seq(SendToUser(user, roomList)))

    case ListMembers(user) =>
      val memberList = userRoom.get(user) match {
        case Some(room) =>
          room
            .users
            .toList
            .mkString("Room Members:\n\t", "\n\t", "")

        case None =>
          "You are not currently in a room"
      }

      (this, Seq(SendToUser(user, memberList)))

    case Disconnect(user) =>
      removeFromCurrentRoom(user)

    case InvalidInput(user, text) =>
      (this, Seq(SendToUser(user, s"Invalid input: $text")))
  }

  private def sendTextToRoom(room: Room, text: String): Seq[OutputMessage] = {
    room
      .users
      .map(SendToUser(_, text))
      .toSeq
  }

  private def updateGameInRoom(room: Room, move: String, user: User): (ChessState, Seq[OutputMessage]) = {
    val updatedGame: Either[Error, GameState] = {
        val game = room.game.get
        val sideToMove = game.gameState.sideToMove
        val moveToMake: Option[Move] = Move.fromString(move)
        (sideToMove, moveToMake) match {
          case (White, Some(value)) if user == game.playerWhite => game.gameState.updateGame(value)
          case (Black, Some(value)) if user == game.playerBlack => game.gameState.updateGame(value)
          case _ => Left(MoveValidationError)
        }
    }
    updatedGame match {
      case Left(_) => (this, Seq(SendToUser(user,"Invalid Move")))
      case Right(value) if value.gameStatus == Continue => {
        val updatedRoom = room.copy(game=Some(room.game.get.copy(gameState=value)))
        val updatedUserRooms = updatedRoom.users.map { _ -> updatedRoom}.toMap
        (this.copy(rooms=rooms + (room.name -> updatedRoom), userRoom = updatedUserRooms), sendTextToRoom(room, value.toFen))
      }
      case Right(value) =>{
        val updatedRoom = room.copy(game=Some(Game(room.game.get.playerWhite, room.game.get.playerBlack, GameState.createGame)))
        val updatedUserRooms = updatedRoom.users.map { _ -> updatedRoom}.toMap
        val finalMessage = s"${value.sideToMove} won!! Try another time, now ${room.game.get.playerWhite.name} plays white and ${room.game.get.playerBlack.name} plays black"
        (this.copy(rooms=rooms + (room.name -> updatedRoom), userRoom = updatedUserRooms), sendTextToRoom(room, finalMessage + '\n' + value.toFen))
      } 
    }
    
  }

  private def sendToUser(user: User, text: String): OutputMessage = {
    SendToUser(user, text)
  }

  private def removeFromCurrentRoom(user: User): (ChessState, Seq[OutputMessage]) = userRoom.get(user) match {
    case Some(room) =>
      val nextMembers = room.users.filter(_ != user)
      val nextState =
        if (nextMembers.isEmpty)
          ChessState(rooms - room.name, userRoom - user, lobby.addToLobby(user))
        else
          ChessState(rooms + (room.name -> room.copy(users=nextMembers)), userRoom - user, lobby.addToLobby(user))

      (nextState, sendTextToRoom(room, s"${user.name} has left ${room.name}"))
    case None =>
      (this, Nil)
  }

  private def addToRoom(user: User, room: String): (ChessState, Seq[OutputMessage]) = {
    
    val roomTo = rooms.getOrElse(room, Room(room, Vector(), None))
    val nextMembers: Vector[User] = roomTo.users :+ user
    if (nextMembers.length > 2){
      (this, Seq(this.sendToUser(user, s"too many players in room $room")))
    }
    else if (nextMembers.length == 2){
      val updatedRoom = roomTo.copy(users=nextMembers, game=Some(Game.createGame(nextMembers)))
      val updatedUserRooms = nextMembers.map { _ -> updatedRoom}.toMap
      val state = ChessState(rooms + (room -> updatedRoom), updatedUserRooms, lobby.removeFromLobby(user))
      (state, state.sendTextToRoom(updatedRoom, s"game has started. ${updatedRoom.game.get.playerWhite.name} is playing white. " +
        s"${updatedRoom.game.get.playerBlack.name} is plaing black" + '\n' + updatedRoom.game.get.gameState.toFen))
    }
    else{
      val updatedRoom = roomTo.copy(users=nextMembers)
      val updatedUserRooms = nextMembers.map { _ -> updatedRoom}.toMap
      val nextState = ChessState(rooms + (room -> updatedRoom), updatedUserRooms, lobby.removeFromLobby(user))
      (nextState, nextState.sendTextToRoom(updatedRoom, s"${user.name} has joined $room"))
    }
    
  }

  private def sendToLobby(text: String): Seq[OutputMessage] = {
    lobby
      .users
      .map(SendToUser(_, text))
      .toSeq
  }

  private def addToLobby(user: User):  (ChessState, Seq[OutputMessage]) = {
    val nextState = ChessState(rooms, userRoom, lobby.addToLobby(user))
    (nextState, nextState.sendToLobby(s"${user.name} has joined lobby"))
  }

}
