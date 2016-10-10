package minotaur.model

import org.specs2.mutable.Specification
import minotaur.io.BoardReader

class MoveSpec extends Specification {
  "Black to move" should {
    val board = BoardReader.fromString("""
      |+   +   +   +   +
      |
      |+---+---+   +   +
      |    | o   x
      |+   +   +   +   +
      |    |   |
      |+   +   +   +   +
      |        |
      |+   +   +   +   +
    """.stripMargin.trim)
    val gs = GameState(
      board, Map(Black -> 4, White -> 3), Black
    )

    "win if he moves north" in {
      val move = PawnMovement(Location(2, board.boardType), gs)
      move.wins === true
    }

    "not win if he moves east" in {
      val move = PawnMovement(Location(7, board.boardType), gs)
      move.wins === false
    }
  }

  "Wall placement" should {
    val board = BoardReader.fromString("""
      |+   +   +   +   +
      |
      |+---+---+   +   +
      |    |
      |+   +   +   +   +
      |    | o | x
      |+   +   +   +   +
      |        |
      |+   +   +   +   +
    """.stripMargin.trim)
    val gs = GameState(
      board, Map(Black -> 4, White -> 3), Black
    )
    val move = WallPlacement(Wall(Location(8, board.boardType), Horizontal), gs)

    "generate the correct new state" in {
      move.play === GameState(
        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    |
          |+   +   +   +   +
          |    | o | x
          |+---+---+   +   +
          |        |
          |+   +   +   +   +
        """.stripMargin.trim),
        Map(Black -> 3, White -> 3),
        White
      )
    }
  }

  "Pawn movement" should {
    val board = BoardReader.fromString("""
      |+   +   +   +   +
      |
      |+---+---+   +   +
      |    |
      |+   +   +   +   +
      |    | o | x
      |+   +   +   +   +
      |        |
      |+   +   +   +   +
    """.stripMargin.trim)
    val gs = GameState(
      board, Map(Black -> 4, White -> 3), Black
    )
    val move = PawnMovement(Location(6, board.boardType), gs)

    "generate the correct new state" in {
      move.play === GameState(
        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    |     x
          |+   +   +   +   +
          |    | o |
          |+   +   +   +   +
          |        |
          |+   +   +   +   +
        """.stripMargin.trim),
        Map(Black -> 4, White -> 3),
        White
      )
    }
  }
}
