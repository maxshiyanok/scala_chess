package chess.side

sealed trait Side {
  def opposite: Side = this match {
    case White => Black
    case Black => White
  }
  val rank: Int
}

case object White extends Side{
  val rank: Int = 0
}
case object Black extends Side{
  val rank: Int = 7
}