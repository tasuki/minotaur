package minotaur.model

import minotaur.io.BoardReader
import org.specs2.mutable.Specification

class GameStateSpec extends Specification {
  "Sample board with black to move" should {
    val board = BoardReader.fromString("""
      |+   +   +   +   +
      |
      |+---+---+   +   +
      |    | o
      |+   +   +   +   +
      |    | x |
      |+   +   +   +   +
      |        |
      |+   +   +   +   +
    """.stripMargin.trim)
    val gs = GameState(
      board, Map(Black -> 4, White -> 3), Black
    )

    "list the possible moves" in {
      gs.getPossibleMoves.filter(_.isValid).map(_.play.board).toSet === Set(
        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    | o   x
          |+   +   +   +   +
          |    |   |
          |+   +   +   +   +
          |        |
          |+   +   +   +   +
        """.stripMargin.trim),

        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    | o
          |+   +   +   +   +
          |    |   |
          |+   +   +   +   +
          |      x |
          |+   +   +   +   +
        """.stripMargin.trim),

        BoardReader.fromString("""
          |+   +   +   +   +
          |            |
          |+---+---+   +   +
          |    | o     |
          |+   +   +   +   +
          |    | x |
          |+   +   +   +   +
          |        |
          |+   +   +   +   +
        """.stripMargin.trim),

        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    | o
          |+   +   +---+---+
          |    | x |
          |+   +   +   +   +
          |        |
          |+   +   +   +   +
        """.stripMargin.trim),

        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    | o     |
          |+   +   +   +   +
          |    | x |   |
          |+   +   +   +   +
          |        |
          |+   +   +   +   +
        """.stripMargin.trim),

        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    | o
          |+   +   +   +   +
          |    | x |
          |+---+---+   +   +
          |        |
          |+   +   +   +   +
        """.stripMargin.trim),

        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    | o
          |+   +   +   +   +
          |    | x |
          |+   +   +---+---+
          |        |
          |+   +   +   +   +
        """.stripMargin.trim),

        BoardReader.fromString("""
          |+   +   +   +   +
          |
          |+---+---+   +   +
          |    | o
          |+   +   +   +   +
          |    | x |   |
          |+   +   +   +   +
          |        |   |
          |+   +   +   +   +
        """.stripMargin.trim)
      )
    }
  }
}
