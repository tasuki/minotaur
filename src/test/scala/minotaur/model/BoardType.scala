package minotaur.model

import org.specs2.mutable.Specification

class BoardTypeSpec extends Specification {
  "Board type locations" should {
    "be 0 to 8 for a 3x3 board" in {
      val boardType = BoardType(size = 3)

      boardType.locations === Vector(
        Location(0, boardType),
        Location(1, boardType),
        Location(2, boardType),
        Location(3, boardType),
        Location(4, boardType),
        Location(5, boardType),
        Location(6, boardType),
        Location(7, boardType),
        Location(8, boardType)
      )
    }

    "be 81 for a 9x9 board" in {
      val boardType = BoardType(size = 9)

      boardType.locations.length === 81
    }
  }

  "Possible walls" should {
    "be on 0, 1, 3, 4 for a 3x3 board" in {
      val bt = BoardType(size = 3)

      bt.possibleWalls === Vector(
        Wall(Location(0, bt), Horizontal),
        Wall(Location(0, bt), Vertical),
        Wall(Location(1, bt), Horizontal),
        Wall(Location(1, bt), Vertical),
        Wall(Location(3, bt), Horizontal),
        Wall(Location(3, bt), Vertical),
        Wall(Location(4, bt), Horizontal),
        Wall(Location(4, bt), Vertical)
      )
    }

    "be 128 for a 9x9 board" in {
      val boardType = BoardType(size = 9)

      boardType.possibleWalls.length === 128
    }
  }
}
