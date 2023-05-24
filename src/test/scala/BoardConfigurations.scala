package chess

import chess.pieces._
import chess.side._

case object BoardConfigurations{
    def captureBoard: Map[Field, Piece] = Map(Field(3, 4)->Rook(White), Field(6, 4)-> Rook(Black))

    def defaultCheck: Map[Field, Piece] = Map(
        Field(3, 4) -> King(White), Field(4, 5) -> Pawn(Black), Field(7, 4) -> Rook(Black),
        Field(3, 0) -> Queen(Black), Field(1, 2) -> Bishop(Black), Field(1, 5) -> Knight(Black),
        Field(0, 0) -> King(Black)
    )

    def coveredCheck: Map[Field, Piece] = Map(
        Field(3, 4) -> King(White), Field(4, 4) -> Pawn(White), Field(7, 4) -> Rook(Black)
    )

    def checkMate: Map[Field, Piece] = Map(
        Field(0, 6) -> King(White), Field(1, 5) -> Pawn(White), Field(1, 6) -> Pawn(White), 
        Field(1, 7) -> Pawn(White), Field(0, 3) -> Queen(Black)
    )

    def checkMateCanCover: Map[Field, Piece] = Map(
        Field(0, 6) -> King(White), Field(1, 5) -> Pawn(White), Field(1, 6) -> Pawn(White), 
        Field(1, 7) -> Pawn(White), Field(0, 3) -> Queen(Black), Field(2, 3) -> Queen(White)
    )

    def newCheckMate: Map[Field, Piece] = Map(
        Field(0, 4) -> King(White), Field(1, 4) -> Queen(White), Field(6, 5) -> Pawn(Black), 
        Field(6, 3) -> Pawn(Black), Field(7, 3) -> Queen(Black), Field(7,5)-> Bishop(Black),
        Field(6,4) -> King(Black)
    )

    def kingSideCastlingNoChecks: Map[Field, Piece] = Map(
        Field(0, 4) -> King(White), Field(0, 7) -> Rook(White)
    )

    def kingSideCastlingWithCheck: Map[Field, Piece] = Map(
        Field(0, 4) -> King(White), Field(0, 7) -> Rook(White), Field(4,6) -> Rook(Black)
    )

    def queenSideCastlingNoChecks: Map[Field, Piece] = Map(
        Field(0, 4) -> King(White), Field(0, 0) -> Rook(White)
    )

    def queenSideCastlingWithCheck: Map[Field, Piece] = Map(
        Field(0, 4) -> King(White), Field(0, 0) -> Rook(White), Field(3,1) -> Bishop(Black)
    )

    def enPassantValid: Map[Field, Piece] = Map(
        Field(3, 3) -> Pawn(White), Field(3, 4) -> Pawn(Black)
    )
    def enPassantinValid: Map[Field, Piece] = Map(
        Field(3, 3) -> Pawn(White), Field(3, 5) -> Pawn(Black), Field(4, 5) -> Bishop(Black)
    )
}