package chess.side

sealed trait Side {
  def opposite: Side = this match {
    case White => Black
    case Black => White
  }
}

case object White extends Side
case object Black extends Side