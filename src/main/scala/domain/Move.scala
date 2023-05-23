package chess

case class Move(from: Field, to: Field){
    def direction: (Int, Int) = (to.rank-from.rank, to.file-from.file)

    def moveVector: (Int, Int) = (direction._1.sign, direction._2.sign)

    def isMoveOnLine: Boolean = direction match {
        case (0, _) | (_, 0) => true
        case (x, y) if Math.abs(x) == Math.abs(y) => true
        case _ => false
    }
}
object Move{
    def fromString(str: String): Option[Move] = {
        val noWhitespaces = str.filterNot(_.isWhitespace)
        if (noWhitespaces.forall(_.isDigit)){
            noWhitespaces.toList match{
                case rankFrom :: fileFrom :: rankTo :: fileTo :: Nil => {
                    val fieldFrom = Field.create(rankFrom.asDigit, fileFrom.asDigit)
                    val fieldTo = Field.create(rankTo.asDigit, fileTo.asDigit)
                    (fieldFrom, fieldTo) match {
                        case (Some(f1), Some(f2)) => Some(Move(f1, f2))
                        case _ => None
                    }
                }
                case _ => None
            }
        }
        else
            None
    }
}