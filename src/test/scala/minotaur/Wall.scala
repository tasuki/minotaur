package minotaur

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
}
