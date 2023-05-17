package chess.pieces

import chess.{Board, Move, Field, Piece}
import chess.side._

trait PieceType{
    def validateMove(board: Board, move: Move): Boolean 

    def getFieldsOnWay(from: Field, to: Field, direction: (Int, Int)): List[Field] = {
            if (from == to){
                Nil
            }
            else{
                val newField = Field.apply(from.rank+direction._1, from.file+direction._2)
                newField :: getFieldsOnWay(newField, to, direction)
            }
        }
}

case object Pawn extends PieceType{
    override def validateMove(board: Board, move: Move): Boolean = {
        
        val from =  move.from
        val to = move.to

        val side: Side = board(from).get.side

        val direction = side match {
            case White => 1
            case Black => -1 
        }

        val validateDefaultMove: Boolean = {
            if (from.file == to.file){
                val movement: Boolean = to.rank - from.rank == direction
                val obstacles: Boolean = board(Field.apply(to.rank, from.file)).isEmpty
                movement && obstacles
            }
            else
                false
        }

        val validateStarterMove: Boolean = {
            val isStarter = side match {
                    case White => from.rank == 1
                    case Black => from.rank == 6 
                }
            
            if (isStarter && from.file == to.file){
                val movement: Boolean = to.rank - from.rank == direction * 2
                val obstacles: Boolean = board(Field.apply(to.rank, from.file)).isEmpty &&
                 board.apply(Field.apply(to.rank-direction, from.file)).isEmpty
                
                movement && obstacles
            }
            else
                false
        }
        
        val validateCapture: Boolean = {
            val capturedPiece: Option[Piece] = board(to)
            capturedPiece match{
                case Some(value) => value.side == side.opposite && Math.abs(to.file - from.file) == 1 && to.rank - from.rank == direction
                case None => false
            }
            
        }
        validateStarterMove || validateDefaultMove || validateCapture
    }
}

case object Knight extends PieceType{
    override def validateMove(board: Board, move: Move): Boolean = {
        val from =  move.from
        val to = move.to

        val side: Side = board(from).get.side

        (Math.abs(from.file-to.file), Math.abs(from.rank-to.rank)) match {
            case (2,1) | (1,2) => board(to) match {
                case Some(value) => value.side == side.opposite
                case _ => true
            }
            case _ => false
        }
    }
}

case object Bishop extends PieceType{
    override def validateMove(board: Board, move: Move): Boolean = {

        val from =  move.from
        val to = move.to

        val side: Side = board(from).get.side

        val direction = (to.rank-from.rank, to.file-from.file)
        if (Math.abs(direction._1) == Math.abs(direction._2)){
            val fieldsToCheck = getFieldsOnWay(from, to, (direction._1.sign, direction._2.sign))
            val nothingOnWay: Boolean = fieldsToCheck.forall(field => board(field).isEmpty)
            val captureOrFree: Boolean = board(to) match {
                case Some(value) => value.side == side.opposite
                case _ => true
            }
            nothingOnWay && captureOrFree
        }
        else
            false
    }
}

case object Rook extends PieceType{
    override def validateMove(board: Board, move: Move): Boolean = {
        val from =  move.from
        val to = move.to

        val side: Side = board(from).get.side

        val direction = (to.rank-from.rank, to.file-from.file)
        direction match {
            case (0, _) | (_, 0) => {
                val fieldsToCheck = getFieldsOnWay(from, to, (direction._1.sign, direction._2.sign))
                val nothingOnWay: Boolean = fieldsToCheck.forall(field => board(field).isEmpty)
                val captureOrFree: Boolean = board(to) match {
                    case Some(value) => value.side == side.opposite
                    case _ => true
                }
                nothingOnWay && captureOrFree
            }
            case _ => false
        }
    }
}

case object Queen extends PieceType{
    override def validateMove(board: Board, move: Move): Boolean = {
        Rook.validateMove(board, move) || Bishop.validateMove(board, move)
    }
}

case object King extends PieceType{
    override def validateMove(board: Board, move: Move): Boolean = {
        val from =  move.from
        val to = move.to

        (Math.abs(to.rank-from.rank), Math.abs(to.file-from.file)) match {
            case (0, 1) | (1, 0) | (1, 1) => Queen.validateMove(board, move) 
            case _ => false
    }
}
}
