package minotaur

import org.specs2.mutable.Specification

class BoardSpec extends Specification {
  "Board type locations" should {
    "be 0 to 8 for a 3x3 board" in {
      val boardType = BoardType(size = 3)

      boardType.locations === Set(
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
  }

  "Possible walls" should {
    "be on 0, 1, 3, 4 for a 3x3 board" in {
      val bt = BoardType(size = 3)

      bt.possibleWalls === Set(
        Wall(Location(0, bt), Horizontal, bt),
        Wall(Location(1, bt), Horizontal, bt),
        Wall(Location(3, bt), Horizontal, bt),
        Wall(Location(4, bt), Horizontal, bt),
        Wall(Location(0, bt), Vertical, bt),
        Wall(Location(1, bt), Vertical, bt),
        Wall(Location(3, bt), Vertical, bt),
        Wall(Location(4, bt), Vertical, bt)
      )
    }
  }
}
