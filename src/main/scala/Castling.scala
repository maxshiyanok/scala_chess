package chess

sealed trait Castling

case object KingSideCastling extends Castling

case object QueenSideCastling extends Castling