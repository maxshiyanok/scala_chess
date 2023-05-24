package chess.pieces

import chess.{Board, Move, Field}
import chess.side._

trait Piece{
    def glider: Boolean

    def side: Side

    def validateMove(board: Board, move: Move): Boolean 

    def getFieldsOnWay(move: Move): List[Field] = {
            if (!move.isMoveOnLine){
                Nil
            }
            else {
                val newField = Field.create(move.from.rank+move.moveVector._1, move.from.file+move.moveVector._2)
                newField match{
                    case Some(value) if value != move.to => value :: getFieldsOnWay(Move(value, move.to))
                    case _ => Nil
                }
            }
        }
    def pieceToFen: Char
}

case class Pawn(side: Side) extends Piece{
    def glider: Boolean = false

    def pawnDirection = side match {
            case White => 1
            case Black => -1 
        }

    override def validateMove(board: Board, move: Move): Boolean = {
        
        val from =  move.from
        val to = move.to

        val isStarterMove = from.rank == side.rank + pawnDirection

        move.direction match {
            case (rankDiff, 0) if rankDiff == pawnDirection => board(to).isEmpty
            case (rankDiff, 0) if isStarterMove && rankDiff == pawnDirection * 2 => List(to,Field(to.rank - pawnDirection, to.file)).forall(board(_).isEmpty)
            case (rankDiff, fileDiff) if rankDiff == pawnDirection && Math.abs(fileDiff) == 1 => {
                board(to) match {
                    case Some(value) => value.side == side.opposite
                    case None => false
                }
            }
            case _ => false
        }
    }

    def pieceToFen: Char = side match {
        case White => 'P'
        case Black => 'p'
    }
}

case class Knight(side: Side) extends Piece{
    def glider: Boolean = false
    override def validateMove(board: Board, move: Move): Boolean = {
        val side: Side = board(move.from).get.side

        (Math.abs(move.direction._1), Math.abs(move.direction._2)) match {
            case (2,1) | (1,2) => board(move.to) match {
                case Some(value) => value.side == side.opposite
                case _ => true
            }
            case _ => false
        }
    }

    def pieceToFen: Char = side match {
        case White => 'N'
        case Black => 'n'
    }
}

case class Bishop(side: Side) extends Piece{
    def glider: Boolean = true
    override def validateMove(board: Board, move: Move): Boolean = {
        val side: Side = board(move.from).get.side

        if (Math.abs(move.direction._1) == Math.abs(move.direction._2)){
            val fieldsToCheck = getFieldsOnWay(move)
            val nothingOnWay: Boolean = fieldsToCheck.forall(field => board(field).isEmpty)
            val captureOrFree: Boolean = board(move.to) match {
                case Some(value) => value.side == side.opposite
                case _ => true
            }
            nothingOnWay && captureOrFree
        }
        else
            false
    }
    def pieceToFen: Char = side match {
        case White => 'B'
        case Black => 'b'
    }
}

case class Rook(side: Side) extends Piece{
    def glider: Boolean = true
    override def validateMove(board: Board, move: Move): Boolean = {
        val side: Side = board(move.from).get.side

        move.direction match {
            case (0, _) | (_, 0) => {
                val fieldsToCheck = getFieldsOnWay(move)
                val nothingOnWay: Boolean = fieldsToCheck.forall(field => board(field).isEmpty)
                val captureOrFree: Boolean = board(move.to) match {
                    case Some(value) => value.side == side.opposite
                    case _ => true
                }
                nothingOnWay && captureOrFree
            }
            case _ => false
        }
    }
    def pieceToFen: Char = side match {
        case White => 'R'
        case Black => 'r'
    }
}

case class Queen(side: Side) extends Piece{
    def glider: Boolean = true
    override def validateMove(board: Board, move: Move): Boolean = {
        Rook(side).validateMove(board, move) || Bishop(side).validateMove(board, move)
    }
    def pieceToFen: Char = side match {
        case White => 'Q'
        case Black => 'q'
    }
}

case class King(side: Side) extends Piece{
    def glider: Boolean = false
    override def validateMove(board: Board, move: Move): Boolean = {

        (Math.abs(move.direction._1), Math.abs(move.direction._2)) match {
            case (0, 1) | (1, 0) | (1, 1) => Queen(side).validateMove(board, move) 
            case _ => false
        }
    }
    def pieceToFen: Char = side match {
            case White => 'K'
            case Black => 'k'
        }
}
