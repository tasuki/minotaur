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
      wall.isNorthmost must beTrue
    }
    "not be southmost" in {
      wall.isSouthmost must beFalse
    }
    "not be eastmost" in {
      wall.isEastmost must beFalse
    }
    "be westmost" in {
      wall.isWestmost must beTrue
    }
  }

  "Bottom right wall" should {
    val wall = Wall(Location(4, BoardType(3)), Horizontal)

    "not be northmost" in {
      wall.isNorthmost must beFalse
    }
    "be southmost" in {
      wall.isSouthmost must beTrue
    }
    "be eastmost" in {
      wall.isEastmost must beTrue
    }
    "not be westmost" in {
      wall.isWestmost must beFalse
    }
  }
}
