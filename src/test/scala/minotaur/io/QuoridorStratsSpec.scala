package minotaur.io

import org.specs2.mutable.Specification

class QuoridorStratsSpec extends Specification {
  "Quoridor strats notation" should {
    "should convert record to Game" in {
      val record = "e2;e8;e3;e7;e4;d7h;e5;f7h;d4h;c5v;f4h;h4v;g4v;h7h;h5h;b7h;b6h;a8h;d6v;c8h;e6h;g6v;e8v"
      val game = QuoridorStrats.importGame(record)

      val previousBoard = BoardReader.fromString("""
        |+   +   +   +   +   +   +   +   +   +
        |
        |+---+---+---+---+   +   +   +   +   +
        |
        |+   +---+---+---+---+---+---+---+---+
        |                | o         |
        |+   +---+---+   +---+---+   +   +   +
        |            |   |           |
        |+   +   +   +   +   +   +   +---+---+
        |            |     x         |   |
        |+   +   +   +---+---+---+---+   +   +
        |                            |   |
        |+   +   +   +   +   +   +   +   +   +
        |
        |+   +   +   +   +   +   +   +   +   +
        |
        |+   +   +   +   +   +   +   +   +   +
        |
        |+   +   +   +   +   +   +   +   +   +
      """.stripMargin.trim)

      val board = BoardReader.fromString("""
        |+   +   +   +   +   +   +   +   +   +
        |                    |
        |+---+---+---+---+   +   +   +   +   +
        |                    |
        |+   +---+---+---+---+---+---+---+---+
        |                | o         |
        |+   +---+---+   +---+---+   +   +   +
        |            |   |           |
        |+   +   +   +   +   +   +   +---+---+
        |            |     x         |   |
        |+   +   +   +---+---+---+---+   +   +
        |                            |   |
        |+   +   +   +   +   +   +   +   +   +
        |
        |+   +   +   +   +   +   +   +   +   +
        |
        |+   +   +   +   +   +   +   +   +   +
        |
        |+   +   +   +   +   +   +   +   +   +
      """.stripMargin.trim)

      game.state.board === board

      game.parent.get.state.board === previousBoard
    }

    "should convert Game to record" in {
      val record = "e2;e8;e3;f8;e4;d4h;d3h;e3v;d4;b4h;b3h;f7;f6h;g7;c4;h6v;g6v;f7;b4"

      val game = QuoridorStrats.importGame(record)
      val exported = QuoridorStrats.exportGame(game)

      exported === record
    }
  }
}
