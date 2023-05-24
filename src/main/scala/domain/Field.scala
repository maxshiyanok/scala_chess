package chess

case class Field(rank: Int, file: Int)

object Field{
    def letterToFile: Map[Char, Int] = {
        Map('a'-> 0, 'b'-> 1, 'c'-> 2, 'd'-> 3, 'e'-> 4, 'f'->5, 'g'-> 6, 'h'-> 7)
    }
    def create(rank: Int, file: Int): Option[Field] = {
        (rank, file) match {
            case (a, _) if a < 0 || a > 7 => None
            case (_, b) if b < 0 || b > 7 => None
            case (a, b) => Some(Field(a, b))
        }         
    }
    def fromString(stringField: String): Option[Field] = {
        stringField.toList match {
            case head :: next :: Nil => {
                val rank: Option[Int] = 
                    if (next.isDigit) Some(next.asDigit) else None
                val file: Option[Int] = letterToFile.get(head)
                (rank, file) match {
                    case (Some(r), Some(f)) => Field.create(r-1, f)
                    case _ => None
                }
            }
            case _ => None
        }
    }
}