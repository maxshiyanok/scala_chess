package chess

import chess.side._
import chess.pieces.{Piece, Rook, King}

sealed trait Castling{

    val side: Side
    val rank: Int = side.rank

    def getFieldsToCheck: List[Field]

    def checkRook(board: Map[Field, Piece]): Boolean

    def checkKingAndRook(board: Map[Field, Piece]): Boolean = {
        board.getOrElse(Field(rank, 4), King(side.opposite)) == King(side) && checkRook(board)
    }

    def validateCastling(move: Move): Boolean 

    def castle: (Move, Move)

    def castlingToFen: String
}

case class KingSideCastling(side: Side) extends Castling {

    def getFieldsToCheck: List[Field] = {
        Field(rank, 4) :: Field(rank, 5) :: Field(rank, 6) :: Nil
    }
    def checkRook(board: Map[Field, Piece]): Boolean = {
        board.getOrElse(Field(rank, 7), Rook(side.opposite)) == Rook(side)
    }

    def validateCastling(move: Move): Boolean = {
        move.from == Field(rank, 4) && move.to == Field(rank, 6)
    }

    def castle: (Move, Move) = {
        (Move(Field(rank, 4), Field(rank, 6)), Move(Field(rank, 7), Field(rank, 5)))
    }

    def castlingToFen: String = side match {
        case White => "K"
        case Black => "k"
    }
}

case class QueenSideCastling(side: Side) extends Castling {

    def getFieldsToCheck: List[Field] = {
        Field(rank, 4) :: Field(rank, 3) :: Field(rank, 2) :: Nil
    }

    def checkRook(board: Map[Field, Piece]): Boolean = {
        board.getOrElse(Field(rank, 0), Rook(side.opposite)) == Rook(side)
    }

    def validateCastling(move: Move): Boolean = {
        move.from == Field(rank, 4) && move.to == Field(rank, 2)
    }

    def castle: (Move, Move) = {
        (Move(Field(rank, 4), Field(rank, 2)), Move(Field(rank, 0), Field(rank, 3)))
    }

    def castlingToFen: String = side match {
        case White => "Q"
        case Black => "q"
    }
}