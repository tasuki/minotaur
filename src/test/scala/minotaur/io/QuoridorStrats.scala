package minotaur.io

import org.specs2.mutable.Specification

class QuoridorStratsSpec extends Specification {
  "Quoridor strats notation game record" should {
    "be converted to Game" in {
      val game = QuoridorStrats.fromString(
        "e2;e8;e3;e7;e4;d7h;e5;f7h;d4h;c5v;f4h;h4v;g4v;h7h;h5h;b7h;b6h;a8h;d6v;c8h;e6h;g6v;e8v"
      )

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
    }
  }
}
