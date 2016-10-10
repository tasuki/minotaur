package minotaur.io

import org.specs2.mutable.Specification

import minotaur.model.{BoardType,Wall,Location}
import minotaur.model.{Horizontal,Vertical}

class CoordinatesSpec extends Specification {
  "Standard board coordinates" should {
    val coords = Coordinates(BoardType(9))

    "allow top left" in {
      coords.exist('a', '1') === true
    }

    "allow mid bottom" in {
      coords.exist('f', '8') === true
    }

    "disallow too far right" in {
      coords.exist('m', '8') === false
    }

    "disallow too low" in {
      coords.exist('a', '9') === false
    }
  }

  "Coordinates" should {
    val bt = BoardType(9)
    val coords = Coordinates(bt)
    "print horizontal wall coords" in {
      coords.forWall(Wall(Location(7, bt), Horizontal)) === "1i"
    }
    "print vertical wall coords" in {
      coords.forWall(Wall(Location(0, bt), Vertical)) === "a1"
    }
    "print bottom right vertical wall coords" in {
      coords.forWall(Wall(Location(70, bt), Vertical)) === "i8"
    }
  }
}
