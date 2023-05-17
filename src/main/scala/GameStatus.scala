package chess

sealed trait GameStatus

case object Win extends GameStatus

case object Draw extends GameStatus

case object Continue extends GameStatus