package chess

import org.scalatest.flatspec.AnyFlatSpec

import chess.side._
import chess.pieces.Pawn

class ChessSpec extends AnyFlatSpec {

  val board = Board(Map()).createBoard()

  val updatedBoard = board.updateBoard(Move(Field(1, 4), Field(3, 4)), White, Nil, None)

  "Board create" should "have size 32" in {
    assert(board.getBoard.size == 32)
  }

  "Pawn move" should "be true" in {
    assert(board.validateMove(Move(Field(1, 4), Field(3, 4)), White) == true)
  }

   "Rook move through piece" should "be false" in {
    assert(board.validateMove(Move(Field(0, 0), Field(4, 0)), White) == false)
  }

  "Knight move" should "be true" in {
    assert(board.validateMove(Move(Field(0, 1), Field(2, 2)), White) == true)
  }

  "Knight move on same piece" should "be false" in {
    assert(board.validateMove(Move(Field(0, 1), Field(1, 3)), White) == false)
  }

  "Bishop move through piece" should "be false" in {
    assert(board.validateMove(Move(Field(0, 5), Field(2, 3)), White) == false)
  }

  "King move to e2" should "be false" in {
    assert(board.validateMove(Move(Field(0, 4), Field(1, 4)), White) == false)
  }

  "Queen move to h4" should "be false" in {
    assert(board.validateMove(Move(Field(0, 3), Field(3, 7)), White) == false)
  }

  "e2-e2 move" should "be valid" in {
    assert(updatedBoard.isEmpty == false)
  }

  "Bishop move in updated position" should "be true" in {
    assert(updatedBoard.get.validateMove(Move(Field(0, 5), Field(2, 3)), White) == true)
  }

  "Queen move to h4 in updated position" should "be true" in {
    assert(updatedBoard.get.validateMove(Move(Field(0, 3), Field(4, 7)), White) == true)
  }

  "Queen move to e3 in updated position" should "be false" in {
    assert(updatedBoard.get.validateMove(Move(Field(0, 3), Field(2, 4)), White) == false)
  }

  "King move to e2 in updated position" should "be true" in {
    assert(updatedBoard.get.validateMove(Move(Field(0, 4), Field(1, 4)), White) == true)
  }

  "Rook capture validation" should "be true" in {
    val captureBoard: Board = Board(BoardConfigurations.captureBoard)
    assert(captureBoard.validateMove(Move(Field(6, 4), Field(3,4)), Black) == true)
  }

  "Validate checks" should "be valid" in {
    val checkBoard: Board = Board(BoardConfigurations.defaultCheck)
    assert(GameEvaluator.piecesAttackingField(checkBoard, Field(3, 4), White).length == 5)
  }

  "Covered checks" should "be valid" in {
    val coveredCheckBoard: Board = Board(BoardConfigurations.coveredCheck)
    assert(GameEvaluator.piecesAttackingField(coveredCheckBoard, Field(3, 4), White).length == 0)
  }

  "Checkmate" should "be valid" in {
    val checkMateBoard: Board = Board(BoardConfigurations.checkMate)
    assert(GameEvaluator.isCheckMate(checkMateBoard, White) == true)
  }

  "False checkmate" should "not be valid" in {
    val falseCheckMateBoard: Board = Board(BoardConfigurations.checkMateCanCover)
    assert(GameEvaluator.isCheckMate(falseCheckMateBoard, White) == false)
  }

  "new false checkmate" should "not be valid" in {
    val falseCheckMateBoard: Board = Board(BoardConfigurations.newCheckMate)
    assert(GameEvaluator.isCheckMate(falseCheckMateBoard, Black) == false)
  }

  "King side castling not checks" should "be valid" in {
    val castlingBoard: Board = Board(BoardConfigurations.kingSideCastlingNoChecks)
    val castlingOptions: List[Castling] = KingSideCastling(White)::Nil
    val updatedBoard: Option[Board] = castlingBoard.updateBoard(Move(Field(0,4), Field(0, 6)), White, castlingOptions, None)
    assert(updatedBoard.isEmpty == false)
  }
  "King side castling with checks" should "not be valid" in {
    val castlingBoard: Board = Board(BoardConfigurations.kingSideCastlingWithCheck)
    val castlingOptions: List[Castling] = KingSideCastling(White)::Nil
    val updatedBoard: Option[Board] = castlingBoard.updateBoard(Move(Field(0,4), Field(0, 6)), White, castlingOptions, None)
    assert(updatedBoard.isEmpty == true)
  }
  "Queen side castling not checks" should "be valid" in {
    val castlingBoard: Board = Board(BoardConfigurations.queenSideCastlingNoChecks)
    val castlingOptions: List[Castling] = KingSideCastling(White)::QueenSideCastling(White)::Nil
    val updatedBoard: Option[Board] = castlingBoard.updateBoard(Move(Field(0,4), Field(0, 2)), White, castlingOptions, None)
    assert(updatedBoard.isEmpty == false)
  }
  "Queen side castling with checks" should "not be valid" in {
    val castlingBoard: Board = Board(BoardConfigurations.queenSideCastlingWithCheck)
    val castlingOptions: List[Castling] = KingSideCastling(White)::QueenSideCastling(White)::Nil
    val updatedBoard: Option[Board] = castlingBoard.updateBoard(Move(Field(0,4), Field(0, 2)), White, castlingOptions, None)
    assert(updatedBoard.isEmpty == true)
  }
  "en passant" should "be valid" in {
    val enPassantBoard: Board = Board(BoardConfigurations.enPassantValid)
    val updatedBoard: Option[Board] = enPassantBoard.updateBoard(Move(Field(3,4), Field(2,3)), Black, Nil, Some(Field(2,3)))
    assert(updatedBoard.isEmpty == false)
    assert(updatedBoard.get.board == Map(Field(2,3) -> Pawn(Black)))
  }
   "wrong en passant" should "not be valid" in {
    val enPassantBoard: Board = Board(BoardConfigurations.enPassantinValid)
    val updatedBoard: Option[Board] = enPassantBoard.updateBoard(Move(Field(3,4), Field(2,3)), Black, Nil, Some(Field(2,3)))
    assert(updatedBoard.isEmpty == true)
  }
  "starter pawn move" should "be true" in {
    assert(board.getEnPassantField(Move(Field(1, 3), Field(3, 3)), White) == Some(Field(2, 3)))
  }

  "GameState creation" should "be valid" in {
    val game: GameState = GameState.createGame
    val updGame = for {
      updatedGame <- game.updateGame(Move(Field(1,4), Field(3, 4)))
      updatedGame1 <- updatedGame.updateGame(Move(Field(6, 4), Field(4, 4)))
      updatedGame2 <- updatedGame1.updateGame(Move(Field(0, 4), Field(1, 4)))
      updatedGame3 <- updatedGame2.updateGame(Move(Field(7, 3), Field(3, 7)))
    } yield updatedGame3
    assert(updGame.isRight == true)
  }
  "GameState checkmate" should "be valid" in {
    val game: GameState = GameState.createGame
    val updGame = for {
      updatedGame <- game.updateGame(Move(Field(1,4), Field(3, 4)))
      updatedGame1 <- updatedGame.updateGame(Move(Field(6, 4), Field(4, 4)))
      updatedGame2 <- updatedGame1.updateGame(Move(Field(0, 3), Field(4, 7)))
      updatedGame3 <- updatedGame2.updateGame(Move(Field(6, 7), Field(5, 7)))
      updatedGame4 <- updatedGame3.updateGame(Move(Field(0, 5), Field(3, 2)))
      updatedGame5 <- updatedGame4.updateGame(Move(Field(7, 1), Field(5, 2)))
      updatedGame6 <- updatedGame5.updateGame(Move(Field(4, 7), Field(6, 5)))
    } yield updatedGame6
    assert(updGame.map(_.gameStatus) == Right(Win))
  }
}