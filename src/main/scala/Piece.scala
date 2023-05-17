package chess

import chess.pieces._
import chess.side._

case class Piece(pieceType: PieceType, side: Side){

    def validateMove(board: Board, move: Move): Boolean = {
        pieceType.validateMove(board, move)
    }
}