package chess

import chess.side._

case class GameState(
    val board: Board, 
    val gameStatus: GameStatus, 
    val sideToMove: Side,
    val castlingWhite: List[Castling], 
    val castlingBlack: List[Castling],
    val enPassantField: Option[Field]){

        def castlingAvailability: (List[Castling], List[Castling]) = {
            (castlingWhite.filter(_.checkKingAndRook(board.getBoard)), 
                        castlingBlack.filter(_.checkKingAndRook(board.getBoard)))
        }

        def updateGame(move: Move): Either[Error, GameState] = {
            val castling: (List[Castling], List[Castling]) = castlingAvailability
            val requiredCastlingOptions = sideToMove match {
                case White => castling._1
                case Black => castling._2
            }
            val updatedBoard: Option[Board] = board.updateBoard(move, sideToMove, requiredCastlingOptions, enPassantField)
            val updatedEnPassantField: Option[Field] = board.getEnPassantField(move, sideToMove)

            updatedBoard match {
                case Some(newBoard) => {
                    if (GameEvaluator.kingUnderCheck(newBoard, sideToMove))
                        Left(MoveCausesCheck)
                    else if (GameEvaluator.isCheckMate(newBoard, sideToMove.opposite))
                        Right(GameState(newBoard, Win, sideToMove, Nil, Nil, None))
                    else
                        Right(GameState(newBoard, Continue, sideToMove.opposite, castling._1, castling._2, updatedEnPassantField))
                }
                case None => Left(MoveValidationError)
            }
        }
        def toFen: (String, String) = {

            def fenLine(tempSum: Int, line: List[Char]): String = {
                line match {
                    case head :: next => {
                        if (head == '-'){
                            fenLine(tempSum+1, next)
                        }
                        else if (tempSum > 0)
                            tempSum.toString + head.toString + fenLine(0, next) 
                        else
                            head.toString + fenLine(0, next)
                    }
                    case Nil if tempSum > 0 => tempSum.toString
                    case _ => ""
                }
            }

            def lineToFen(rank: Int): String = {
                val myBoard = board.getBoard
                val bitLine = List.range(0,8)
                val fenList: List[Char] = bitLine.map(file => {
                    val pieceOption = myBoard.get(Field(rank, file))
                    pieceOption match {
                        case Some(value) => value.pieceToFen
                        case _ => '-'
                    }
                })
                //fenLine(0, fenList)
                fenList.mkString
            }

            val fenBitBoard = List.range(0,8).map(lineToFen(_))
            val fenBitBoardWhite = fenBitBoard.reverse.mkString("\n")
            val fenBitBoardBlack = fenBitBoard.map(_.reverse).mkString("\n")
            val fenSideToMove = sideToMove match {
                case White => "w"
                case Black => "b"
            }
            val fenCastling = castlingWhite.map(_.castlingToFen).mkString + castlingBlack.map(_.castlingToFen).mkString
            val checkedFenCastling = if (fenCastling.isBlank) "-" else fenCastling
            val fenEnPassant = "-"
            val fenHalfMoves = "0"
            val fenMoves = "0"
            (s"$fenBitBoardWhite $fenSideToMove $checkedFenCastling $fenEnPassant $fenHalfMoves $fenMoves",
            s"$fenBitBoardBlack $fenSideToMove $checkedFenCastling $fenEnPassant $fenHalfMoves $fenMoves")
        }
    }

case object GameState{
    def createGame: GameState = {
            val board = Board(Map()).createBoard()
            val gameStatus = Continue
            val sideToMove = White
            val castlingWhite = KingSideCastling(White) :: QueenSideCastling(White) :: Nil
            val castlingBlack = KingSideCastling(Black) :: QueenSideCastling(Black) :: Nil
            val enPassantField = None
            GameState(board, gameStatus, sideToMove, castlingWhite, castlingBlack, enPassantField)
        }
}