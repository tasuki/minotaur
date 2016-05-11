package minotaur.model

import org.specs2.mutable.Specification

class WallSpec extends Specification {
  "Horizontal" should {
    "have vertical as its opposite" in {
      Vertical.opposite === Horizontal
    }
  }

  "Vertical" should {
    "have horizontal as its opposite" in {
      Horizontal.opposite === Vertical
    }
  }

  "Too much to the east wall" should {
    "throw an exception" in {
      Wall(Location(2, BoardType(3)), Horizontal) must throwA[IllegalArgumentException]
    }
  }

  "Too much to the south wall" should {
    "throw an exception" in {
      Wall(Location(7, BoardType(3)), Horizontal) must throwA[IllegalArgumentException]
    }
  }

  "Top left wall" should {
    val wall = Wall(Location(0, BoardType(3)), Horizontal)

    "be northmost" in {
      wall.borders(North) must beTrue
    }
    "not be southmost" in {
      wall.borders(South) must beFalse
    }
    "not be eastmost" in {
      wall.borders(East) must beFalse
    }
    "be westmost" in {
      wall.borders(West) must beTrue
    }
  }

  "Bottom right wall" should {
    val wall = Wall(Location(4, BoardType(3)), Horizontal)

    "not be northmost" in {
      wall.borders(North) must beFalse
    }
    "be southmost" in {
      wall.borders(South) must beTrue
    }
    "be eastmost" in {
      wall.borders(East) must beTrue
    }
    "not be westmost" in {
      wall.borders(West) must beFalse
    }
  }

  "Upper left vertical wall" should {
    val wall = Wall(Location(0, BoardType(3)), Vertical)

    "block crossing and neighbor" in {
      wall.blocks === List(
        Wall(Location(0, BoardType(3)), Horizontal),
        Wall(Location(3, BoardType(3)), Vertical)
      )
    }
  }

  "Bottom right horizontal wall" should {
    val wall = Wall(Location(4, BoardType(3)), Horizontal)

    "block crossing and neighbor" in {
      wall.blocks === List(
        Wall(Location(4, BoardType(3)), Vertical),
        Wall(Location(3, BoardType(3)), Horizontal)
      )
    }
  }

  "Somewhere in the middle wall" should {
    val wall = Wall(Location(21, BoardType(9)), Horizontal)

    "block crossing and its neighbors" in {
      wall.blocks === List(
        Wall(Location(21, BoardType(9)), Vertical),
        Wall(Location(22, BoardType(9)), Horizontal),
        Wall(Location(20, BoardType(9)), Horizontal)
      )
    }
  }
}
