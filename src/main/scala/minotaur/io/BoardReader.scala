package minotaur.io

import scala.io.Source

import minotaur.model._

object BoardReader {
  def fromFile(file: String): Board = {
    fromString(Source.fromFile(file).mkString)
  }

  def fromString(str: String): Board = {
    val fields: List[List[String]] = str.split("\\n").toList
      .map(_.grouped(4).toList)

    val boardType = BoardType((fields.length - 1) / 2)

    val oddRows = fields.sliding(1, 2).flatten.toList
    val evenRows = fields.drop(1).sliding(1, 2).flatten.toList

    val horizontalWalls = getHorizontalWalls(oddRows, boardType)
    val verticalWalls = getVerticalWalls(evenRows, boardType)

    require((
      horizontalWalls.map(_.location) intersect
        verticalWalls.map(_.location)
      ).isEmpty,
    "Walls cross each other")

    Board(
      boardType,
      Player.all.map(
        player => getPawnLocation(player.pawn, evenRows, boardType) -> player
      ).toMap,
      horizontalWalls ++ verticalWalls
    )
  }

  private def getLocations(
    rows: List[List[String]],
    condition: String => Boolean,
    boardType: BoardType
  ): Seq[Location] = {
    val indexed = rows.map(_.zipWithIndex).zipWithIndex

    for {
      (line, row) <- indexed
      (field, column) <- line
      if condition(field)
    } yield Location(row * boardType.size + column, boardType)
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
        first.neighbor(direction).contains(second),
        s"$orientation walls do not form proper pairs"
      )
    }

    pairs.map(_.head)
      .map(Wall(_, orientation)).toSet
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
      _ == "+---",
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
      evenRows.map(_.drop(1)),
      _.head == '|',
      boardType
    ).sortWith((a, b) => {
      val aColumn = a.location % boardType.size
      val bColumn = b.location % boardType.size

      if (aColumn < bColumn) true
      else if (aColumn > bColumn) false
      else a.location < b.location
    })

    getWalls(wallLocations, South, Vertical)
  }

  private def getPawnLocation(
    pawn: Char,
    evenRows: List[List[String]],
    boardType: BoardType
  ): Location = {
    val pawnLocations = getLocations(
      evenRows,
      f => f.length > 2 && f.charAt(2) == pawn,
      boardType
    )

    require(pawnLocations.length == 1, s"There needs to be exactly one pawn $pawn")

    pawnLocations.head
  }
}
