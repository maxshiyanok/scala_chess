package chess

import scala.collection.immutable.{Map, Iterable}

class Board(board: Map[Field, Piece]) {

    def apply(field: Field): Option[Piece] = board.get(field)

    def validateMove(move: Move): Boolean = {
        val piece: Option[Piece] = board.get(move.from)
        
        piece match {
            case Some(value) => value.validateMove(this, move)
            case None => false
        }
    }

    def createBoard() = {
        new Board(Map()) // not implemented
    }

    def updateBoard(move: Move): Either[String, Board] = {
        validateMove(move) match {
            case true => {
                Right(new Board(board + (move.to -> board.get(move.from).get) - move.from))
            }
            case false => Left("Move validation error") // not finished
        }
    }

    def getPieceBoard(): Map[Piece, Iterable[Field]] = {
        board.groupMap(_._2)(_._1)
    }
}