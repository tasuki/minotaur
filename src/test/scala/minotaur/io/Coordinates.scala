package minotaur.io

import org.specs2.mutable.Specification

import minotaur.model.BoardType

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
}
