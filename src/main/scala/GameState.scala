package chess

import chess.side._

class GameState(
    var board: Board, 
    var gameStatus: GameStatus, 
    var sideToMove: Side,
    var castlingWhite: List[Castling], 
    var castlingBlack: List[Castling],
    var enPassantField: Option[Field]){
        def createGame(){
            board = new Board(Map()).createBoard()
            gameStatus = Continue
            sideToMove = White
            castlingWhite = KingSideCastling :: QueenSideCastling :: Nil
            castlingBlack = KingSideCastling :: QueenSideCastling :: Nil
            enPassantField = None
        }

        def updateGame(move: Move) = { // not finished
            for {
                b <- board.updateBoard(move)
            } yield new GameState(b, gameStatus, sideToMove, castlingWhite, castlingBlack, enPassantField)
        }
    }