package chess

case class Field(rank: Int, file: Int)

object Field{
    def create(rank: Int, file: Int): Option[Field] = {
        (rank, file) match {
            case (a, _) if a < 0 || a > 7 => None
            case (_, b) if b < 0 || b > 7 => None
            case (a, b) => Some(new Field(a, b))
        }         
    }
}