package chess

import chess.side._
import chess.pieces._

object GameEvaluator{

    val directions = List((1,0), (1,-1), (0,-1), (-1,-1), (-1, 0), (-1,1), (0,1), (1,1))

    def getAllFieldsByDirection(startField: Field, direction: (Int, Int)): List[Field] = {
        val newField: Option[Field] = Field.create(startField.rank+direction._1, startField.file+direction._2)
        newField match {
            case Some(value) => value :: getAllFieldsByDirection(value, direction)
            case None => Nil
        }
    }

    def piecesCheckingKing(board: Board, side: Side): List[Field] = {
        val pieceBoard: Map[Piece, Iterable[Field]] = board.getPieceBoard()
        val kingField: Field = pieceBoard.get(new Piece(King, side)).get.head

        val enemyKnightsPositions = pieceBoard.get(new Piece(Knight, side.opposite))

        val knightsAttacking = enemyKnightsPositions match {
            case Some(value) => for {
                knightPosition <- value.toList
                knight <- board.apply(knightPosition)
                if knight.validateMove(board, new Move(knightPosition, kingField))
            } yield knightPosition
            case None => Nil
        }

        val piecesAttacking = for {
            direction <- directions
            fieldsByDirection <- getAllFieldsByDirection(kingField, direction)
            pieceOnField <- board.apply(fieldsByDirection) 
            if pieceOnField.side == side.opposite && pieceOnField.validateMove(board, new Move(fieldsByDirection, kingField))
        } yield fieldsByDirection

        knightsAttacking ++ piecesAttacking
    }


}