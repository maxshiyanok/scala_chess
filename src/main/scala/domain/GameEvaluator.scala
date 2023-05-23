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

    def piecesAttackingField(board: Board, fieldToCheck: Field, side: Side): List[Field] = {
        val pieceBoard: Map[Piece, Iterable[Field]] = board.getPieceBoard

        val enemyKnightsPositions = pieceBoard.get(Knight(side.opposite))

        val knightsAttacking = enemyKnightsPositions match {
            case Some(value) => for {
                knightPosition <- value.toList
                if board.validateMove(Move(knightPosition, fieldToCheck), side.opposite)
            } yield knightPosition
            case None => Nil
        }

        val piecesAttacking = for {
            direction <- directions
            fieldsByDirection <- getAllFieldsByDirection(fieldToCheck, direction)
            pieceOnField <- board.apply(fieldsByDirection) 
            if pieceOnField.side == side.opposite && board.validateMove(Move(fieldsByDirection, fieldToCheck), side.opposite)
        } yield fieldsByDirection

        knightsAttacking ++ piecesAttacking
    }

    def kingUnderCheck(board: Board, side: Side): Boolean = {
        val kingField: Field = board.getPieceBoard.get(King(side)).get.head
        piecesAttackingField(board, kingField, side).length != 0
    }

    def isCheckMate(board: Board, side: Side): Boolean = {

        def kingCanCover(checkField: Field, kingField: Field): Boolean = {
            
            val direction = ((kingField.rank - checkField.rank).sign, (kingField.file - checkField.file).sign)
            val pieceAttacking: Piece = board(checkField).get
            val allFieldsOnWay = pieceAttacking.glider match {
                case true => checkField :: pieceAttacking.getFieldsOnWay(Move(checkField, kingField))
                case false => List(checkField)
            }
                 
            val blockCheckOrCapture = for {
                piecesToSaveTheKing <- board.getBoard
                if piecesToSaveTheKing._2.side == side && piecesToSaveTheKing._2 != King(side)
                fieldToCover <- allFieldsOnWay
            } yield board.validateMove(Move(piecesToSaveTheKing._1, fieldToCover), side)

            !blockCheckOrCapture.forall(_ == false)
        }

        def kingCantEscape(kingField: Field): Boolean = {

            val updatedPositionChecks: List[List[Field]] = for {
                direction <- directions
                fieldToEscape <- Field.create(direction._1, direction._2)
                if board.validateMove(Move(kingField, fieldToEscape), side)
                newBoard <- board.updateBoard(Move(kingField, fieldToEscape), side, Nil, None)
            } yield this.piecesAttackingField(newBoard, kingField, side)

            updatedPositionChecks.forall(!_.isEmpty)
        }

        val kingField: Field = board.getPieceBoard.get(King(side)).get.head

        val piecesCheckingKing = piecesAttackingField(board, kingField, side)

        piecesCheckingKing.length match {
            case 2 | 1 if !kingCanCover(piecesCheckingKing.head, kingField) => kingCantEscape(kingField)
            case _ => false
        }
    }
}