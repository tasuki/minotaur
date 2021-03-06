package minotaur.model

import minotaur.io.BoardReader
import org.specs2.mutable.Specification

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

      board.possibleMoves(Black) === Seq(
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

      board.possibleMoves(Black) === Seq(
        Location( 1, board.boardType),
        Location(13, board.boardType),
        Location( 8, board.boardType)
      )
    }

    "jump sideways if straight jump is impossible" in {
      val board = BoardReader.fromString("""
        |+   +   +   +   +
        |
        |+   +---+---+   +
        |      o
        |+   +   +   +   +
        |      x |
        |+   +   +   +   +
        |        |
        |+   +   +   +   +
      """.stripMargin.trim)

      board.possibleMoves(Black) === Seq(
        Location( 6, board.boardType),
        Location( 4, board.boardType),
        Location(13, board.boardType),
        Location( 8, board.boardType)
      )
    }

    "don't jump sideways over walls" in {
      val board = BoardReader.fromString("""
        |+   +   +   +   +
        |
        |+   +---+---+   +
        |    | o
        |+   +   +   +   +
        |    | x |
        |+   +   +   +   +
        |        |
        |+   +   +   +   +
      """.stripMargin.trim)

      board.possibleMoves(Black) === Seq(
        Location( 6, board.boardType),
        Location(13, board.boardType)
      )
    }
  }

  "Available walls" should {
    "be listed" in {
      val board = BoardReader.fromString("""
        |+   +   +   +   +
        |
        |+   +---+---+   +
        |    | o
        |+   +   +   +   +
        |    | x |
        |+   +   +   +   +
        |        |
        |+   +   +   +   +
      """.stripMargin.trim)
      val bt = board.boardType

      board.placeableWalls === Set(
        Wall(Location(2, bt), Vertical),
        Wall(Location(5, bt), Horizontal),
        Wall(Location(6, bt), Vertical),
        Wall(Location(6, bt), Horizontal),
        Wall(Location(8, bt), Horizontal),
        Wall(Location(10, bt), Horizontal),
        Wall(Location(10, bt), Vertical)
      )
    }
  }
}
