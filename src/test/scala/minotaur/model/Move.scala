package minotaur.model

import org.specs2.mutable.Specification
import minotaur.io.BoardReader

class MoveSpec extends Specification {
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
      gs.getPossibleMoves.map(_.apply(gs))
        .map(_.board) === Set(
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
