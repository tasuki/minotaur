package minotaur

import org.specs2.mutable.Specification

class LocationSpec extends Specification {
  "Out of bounds location" should {
    "throw an exception when location is before the board" in {
      Location(location = -1, BoardType(3)) must throwA[IllegalArgumentException]
    }

    "throw an exception when location is after the board" in {
      Location(location = 9, BoardType(3)) must throwA[IllegalArgumentException]
    }
  }

  "Top left corner" should {
    val location = Location(location=0, BoardType(3))

    "be north border" in {
      location.isNorthBorder must beTrue
    }
    "not be south border" in {
      location.isSouthBorder must beFalse
    }
    "not be east border" in {
      location.isEastBorder must beFalse
    }
    "be west border" in {
      location.isWestBorder must beTrue
    }
  }

  "Top right corner" should {
    val location = Location(location=2, BoardType(3))

    "be north border" in {
      location.isNorthBorder must beTrue
    }
    "not be south border" in {
      location.isSouthBorder must beFalse
    }
    "be east border" in {
      location.isEastBorder must beTrue
    }
    "not be west border" in {
      location.isWestBorder must beFalse
    }
  }

  "Bottom left corner" should {
    val location = Location(location=6, BoardType(3))

    "not be north border" in {
      location.isNorthBorder must beFalse
    }
    "be south border" in {
      location.isSouthBorder must beTrue
    }
    "not be east border" in {
      location.isEastBorder must beFalse
    }
    "be west border" in {
      location.isWestBorder must beTrue
    }
  }

  "Bottom right corner" should {
    val location = Location(location=8, BoardType(3))

    "not be north border" in {
      location.isNorthBorder must beFalse
    }
    "be south border" in {
      location.isSouthBorder must beTrue
    }
    "be east border" in {
      location.isEastBorder must beTrue
    }
    "not be west border" in {
      location.isWestBorder must beFalse
    }
  }

  "Middle location" should {
    val location = Location(location=4, BoardType(3))

    "not be north border" in {
      location.isNorthBorder must beFalse
    }
    "not be south border" in {
      location.isSouthBorder must beFalse
    }
    "not be east border" in {
      location.isEastBorder must beFalse
    }
    "not be west border" in {
      location.isWestBorder must beFalse
    }
  }
}
