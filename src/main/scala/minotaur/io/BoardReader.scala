package minotaur.io

import scala.io.Source
import minotaur.model.{BoardType,Location,Wall,Board}
import minotaur.model.{Orientation,Horizontal,Vertical}
import minotaur.model.{Direction,North,South,East,West}

object BoardReader {
  def fromFile(file: String) = {
    fromString(Source.fromFile(file).mkString)
  }

  def fromString(str: String) = {
    val fields: List[List[String]] = str.split("\\n").toList
      .map(line => line.grouped(4).toList)

    val boardType = BoardType((fields.length - 1) / 2)

    val oddRows = fields.sliding(1, 2).flatten.toList
    val evenRows = fields.drop(1).sliding(1, 2).flatten.toList

    val horizontalWalls = getHorizontalWalls(oddRows, boardType)
    val verticalWalls = getVerticalWalls(evenRows, boardType)

    require((
      horizontalWalls.map(w => w.location) intersect
      verticalWalls.map(w => w.location)
    ).size == 0,
    "Walls cross each other")

    Board(
      boardType,
      getPawnLocation('x', evenRows, boardType),
      getPawnLocation('o', evenRows, boardType),
      horizontalWalls ++ verticalWalls
    )
  }

  private def getLocations(
    rows: List[List[String]],
    condition: String => Boolean,
    boardType: BoardType
  ): Seq[Location] = {
    val indexed = rows.map(line => line.zipWithIndex).zipWithIndex

    for {
      (line, row) <- indexed
      (field, column) <- line
      if (condition(field))
    } yield (Location(row * boardType.size + column, boardType))
  }

  private def getWalls(
    wallLocations: Seq[Location],
    direction: Direction,
    orientation: Orientation
  ): Set[Wall] = {
    val pairs = wallLocations.grouped(2).toList
    for (pair <- pairs) require(
      pair.length == 2,
      s"$orientation walls do not form proper pairs"
    )

    for (List(first, second) <- pairs) {
      require(
        first.neighbor(direction) == Some(second),
        s"$orientation walls do not form proper pairs"
      )
    }

    pairs.map(pair => pair(0))
      .map(l => Wall(l, orientation)).toSet
  }

  private def getHorizontalWalls(
    oddRows: List[List[String]],
    boardType: BoardType
  ) = {
    for (line <- oddRows) {
      require(
        line.length == boardType.size + 1,
        "The board should be square"
      )
    }

    val wallLocations = getLocations(
      oddRows.drop(1),
      _ == ".___",
      boardType
    )

    getWalls(wallLocations, East, Horizontal)
  }

  private def getVerticalWalls(
    evenRows: List[List[String]],
    boardType: BoardType
  ) = {
    for (line <- evenRows) {
      require(
        line.length <= boardType.size + 1,
        "Even row board line too long"
      )
    }

    val wallLocations = getLocations(
      evenRows.map(row => row.drop(1)),
      _.head == '|',
      boardType
    ).sortWith((a, b) => {
      val aColumn = a.location % boardType.size
      val bColumn = b.location % boardType.size

      if (aColumn < bColumn) true
      else if (bColumn > aColumn) false
      else a.location < b.location
    })

    getWalls(wallLocations, South, Vertical)
  }

  private def getPawnLocation(
    sign: Char,
    evenRows: List[List[String]],
    boardType: BoardType
  ): Location = {
    val pawnLocations = getLocations(
      evenRows,
      f => f.length > 2 && f.charAt(2) == sign,
      boardType
    )

    require(pawnLocations.length == 1, s"There needs to be exactly one pawn $sign")

    pawnLocations(0)
  }
}
