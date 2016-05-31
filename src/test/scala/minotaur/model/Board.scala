package minotaur.model

import org.specs2.mutable.Specification
import minotaur.io.BoardReader

class BoardSpec extends Specification {
  "Sample board" should {
    val file = "src/test/resources/board.txt"
    val board = BoardReader.fromFile(file)

    "not allow white moving north off the board" in {
      board.canMove(board.pawnLocation(White), North) === false
    }
    "allow white moving south to a free space" in {
      board.canMove(board.pawnLocation(White), South) === true
    }
    "allow white moving east to a free space" in {
      board.canMove(board.pawnLocation(White), East) === true
    }
    "allow white moving west to a free space" in {
      board.canMove(board.pawnLocation(White), West) === true
    }

    "not allow black moving north through a wall" in {
      board.canMove(board.pawnLocation(Black), North) === false
    }
    "allow black moving south to a free space" in {
      board.canMove(board.pawnLocation(Black), South) === true
    }
    "not allow black moving east through a wall" in {
      board.canMove(board.pawnLocation(Black), East) === false
    }
    "allow black moving west to a free space" in {
      board.canMove(board.pawnLocation(Black), West) === true
    }
  }

  "A pawn" should {
    "be able to move to neighboring places unless blocked by a wall" in {
      val board = BoardReader.fromString("""
        |+   +   +   +   +
        |      o
        |+   +   +   +   +
        |
        |+   +   +   +   +
        |      x |
        |+   +   +   +   +
        |        |
        |+   +   +   +   +
      """.stripMargin.trim)

      board.availableMoves(Black) === Seq(
        Location( 5, board.boardType),
        Location(13, board.boardType),
        Location( 8, board.boardType)
      )
    }

    "jump over the opponent" in {
      val board = BoardReader.fromString("""
        |+   +   +   +   +
        |
        |+   +   +   +   +
        |      o
        |+   +   +   +   +
        |      x |
        |+   +   +   +   +
        |        |
        |+   +   +   +   +
      """.stripMargin.trim)

      board.availableMoves(Black) === Seq(
        Location( 1, board.boardType),
        Location(13, board.boardType),
        Location( 8, board.boardType)
      )
    }
  }
}
