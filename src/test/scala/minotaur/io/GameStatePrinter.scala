package minotaur.io

import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.specs2.mutable.Specification

import minotaur.model.{GameState,Black,White}


class GameStatePrinterSpec extends Specification {
  private def arrFromStr(str: String): Array[Float] =
    str.toArray.filter(_.isDigit).map(_.asDigit.toFloat)

  private def ndFromString(str: String): INDArray =
    Nd4j.create(arrFromStr(str), Array(6, 9, 9))

  "State from sample board" should {
    val file = "src/test/resources/board.txt"
    val board = BoardReader.fromFile(file)
    val gs = GameState(
      board, Map(Black -> 3, White -> 9), Black
    )

    "get printed succinctly" in {
      GameStatePrinter.succinct(gs) === "x:58:3,o:4:9,1i2a3b6d7a7i8a:a1i2b7f7i8"
    }

    "retrieve a nd array for network" in {
      val expected = ndFromString(
      """
        |000000010
        |100000000
        |010000000
        |000000000
        |000000000
        |000100000
        |100000010
        |100000000
        |000000000
        |
        |100000000
        |000000010
        |000000000
        |000000000
        |000000000
        |000000000
        |010010000
        |000000010
        |000000000
        |
        |000000000
        |000000000
        |000000000
        |000000000
        |000000000
        |000000000
        |000010000
        |000000000
        |000000000
        |
        |333333333
        |333333333
        |333333333
        |333333333
        |333333333
        |333333333
        |333333333
        |333333333
        |333333333
        |
        |000010000
        |000000000
        |000000000
        |000000000
        |000000000
        |000000000
        |000000000
        |000000000
        |000000000
        |
        |999999999
        |999999999
        |999999999
        |999999999
        |999999999
        |999999999
        |999999999
        |999999999
        |999999999
      """
      )

      GameStatePrinter.toNdArray(gs) === expected
      1 === 1
    }
  }
}
