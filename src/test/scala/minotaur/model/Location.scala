package minotaur.model

import org.specs2.mutable.Specification

class LocationSpec extends Specification {
  "Out of bounds location" should {
    "throw an exception when location is before the board" in {
      Location(-1, BoardType(3)) must throwA[IllegalArgumentException]
    }

    "throw an exception when location is after the board" in {
      Location(9, BoardType(3)) must throwA[IllegalArgumentException]
    }
  }

  "Top left corner" should {
    val location = Location(0, BoardType(3))

    "be north border" in {
      location.isBorder(North) must beTrue
    }
    "not be south border" in {
      location.isBorder(South) must beFalse
    }
    "not be east border" in {
      location.isBorder(East) must beFalse
    }
    "be west border" in {
      location.isBorder(West) must beTrue
    }

    "should get no northern neighbor" in {
      location.neighbor(North) must beNone
    }
    "should get southern neighbor" in {
      location.neighbor(South) === Some(Location(3, BoardType(3)))
    }
  }

  "Top right corner" should {
    val location = Location(2, BoardType(3))

    "be north border" in {
      location.isBorder(North) must beTrue
    }
    "not be south border" in {
      location.isBorder(South) must beFalse
    }
    "be east border" in {
      location.isBorder(East) must beTrue
    }
    "not be west border" in {
      location.isBorder(West) must beFalse
    }
  }

  "Bottom left corner" should {
    val location = Location(6, BoardType(3))

    "not be north border" in {
      location.isBorder(North) must beFalse
    }
    "be south border" in {
      location.isBorder(South) must beTrue
    }
    "not be east border" in {
      location.isBorder(East) must beFalse
    }
    "be west border" in {
      location.isBorder(West) must beTrue
    }
  }

  "Bottom right corner" should {
    val location = Location(8, BoardType(3))

    "not be north border" in {
      location.isBorder(North) must beFalse
    }
    "be south border" in {
      location.isBorder(South) must beTrue
    }
    "be east border" in {
      location.isBorder(East) must beTrue
    }
    "not be west border" in {
      location.isBorder(West) must beFalse
    }

    "should get northern neighbor" in {
      location.neighbor(North) === Some(Location(5, BoardType(3)))
    }
    "should get no southern neighbor" in {
      location.neighbor(South) must beNone
    }
  }

  "Middle location" should {
    val location = Location(4, BoardType(3))

    "not be north border" in {
      location.isBorder(North) must beFalse
    }
    "not be south border" in {
      location.isBorder(South) must beFalse
    }
    "not be east border" in {
      location.isBorder(East) must beFalse
    }
    "not be west border" in {
      location.isBorder(West) must beFalse
    }
  }

  "A location near the top of the board" should {
    val location = Location(21, BoardType(9))

    "be 2 steps from the north border" in {
      location.estimateDistance(North) === 2
    }

    "be 6 steps from the south border" in {
      location.estimateDistance(South) === 6
    }

    "be 5 steps from the east border" in {
      location.estimateDistance(East) === 5
    }

    "be 3 steps from the west border" in {
      location.estimateDistance(West) === 3
    }
  }
}
