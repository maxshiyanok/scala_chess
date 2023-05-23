package chess

import scala.collection.immutable.{Map, Iterable}
import chess.side._
import chess.pieces._

case class Board(board: Map[Field, Piece]) {

    def apply(field: Field): Option[Piece] = board.get(field)

    def validateMove(move: Move, sideToMove: Side): Boolean = {
        val piece: Option[Piece] = board.get(move.from)
        
        piece match {
            case Some(movingPiece) if movingPiece.side == sideToMove => movingPiece.validateMove(this, move)
            case _ => false
        }
    }

    def validateCastling(move: Move, castlingOptions: List[Castling]): Option[Castling] = {
        val validatedCastling: List[Castling] = for {
            castling <- castlingOptions
            if castling.validateCastling(move)
            fieldsToCheck <- castling.getFieldsToCheck
            if GameEvaluator.piecesAttackingField(this, fieldsToCheck, castling.side).isEmpty
        } yield castling

        if (validatedCastling.length == 3)
            Some(validatedCastling.head)
        else
            None
    }

    def validateEnPassant(move: Move, side: Side, enPassantField: Option[Field]): Boolean = {
        val movingPiece: Option[Piece] = board.get(move.from)

        (movingPiece, enPassantField) match {
            case (Some(Pawn(side)), Some(field)) if move.to == field => {
                move.direction._1 == Pawn(side).pawnDirection && Math.abs(move.direction._2) == 1
            }
            case _ => false
        }
    }

    def getEnPassantField(move: Move, side: Side): Option[Field] = {
        val movingPiece: Option[Piece] = this(move.from)
        movingPiece match {
            case Some(Pawn(side)) => {
                val pawnDirection = Pawn(side).pawnDirection

                val isStarterMove = move.from.rank == side.rank + pawnDirection

                move.direction match {
                    case (rankDiff, 0) if isStarterMove && rankDiff == pawnDirection * 2 => 
                        Some(Field(move.from.rank+pawnDirection, move.from.file))
                    case _ => None
                }
            }
            case _ => None
        }
    }

    def createBoard(): Board = {
        def boardMap(side: Side): Map[Field, Piece] = {
            val sidePieceRank: Int = side match {
                case White => 0
                case Black => 7
            }
            val sidePawnRank: Int = side match {
                case White => 1
                case Black => 6
            }
            val picecesInOrder: List[Piece] = Rook(side)::Knight(side)::Bishop(side)::Queen(side)::King(side)::Bishop(side)::Knight(side)::Rook(side)::Nil
            val pieceMap: Map[Field, Piece] = List.range(0, 8).map(field => (Field(sidePieceRank, field))).zip(picecesInOrder).toMap
            val pawnMap: Map[Field, Piece] = List.range(0, 8).map(field => (Field(sidePawnRank, field), Pawn(side))).toMap
            pieceMap ++ pawnMap
        }
        new Board(boardMap(White)++boardMap(Black))
    }

    def capturePiece(field: Field): Board = {
        Board(board - field)
    }

    def makeMoveOnBoard(move: Move): Board = {
        board(move.from) match {
            case Pawn(side) if move.to.rank == side.opposite.rank => Board(board + (move.to -> Queen(side)) - move.from)
            case _ => Board(board + (move.to -> board.get(move.from).get) - move.from)
        }
        
    }

    def updateBoard(move: Move, sideToMove: Side, castlingOptions: List[Castling], enPassantField: Option[Field]): Option[Board]= {
        if (validateMove(move, sideToMove)) {
            Some(makeMoveOnBoard(move))
        }
        else if (validateEnPassant(move, sideToMove, enPassantField)){
            val pawnDirection = sideToMove match {
                    case White => -1
                    case Black => 1 
                }
            Some(makeMoveOnBoard(move).capturePiece(Field(move.to.rank+pawnDirection, move.to.file)))
        }
        else {
            validateCastling(move, castlingOptions) match {
                case Some(castling) => {
                    val kingAndRookMoves: (Move, Move) = castling.castle
                    Some(makeMoveOnBoard(kingAndRookMoves._1).makeMoveOnBoard(kingAndRookMoves._2))
                }
                case _ => None
            }
        }
    }

    def getPieceBoard: Map[Piece, Iterable[Field]] = {
        board.groupMap(_._2)(_._1)
    }

    def getBoard: Map[Field, Piece] = {
        board
    }
}