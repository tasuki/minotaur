package minotaur.model

case class BoardType(size: Int = 9) {
  // used to verify location validity
  val possibleLocations: Vector[Int] = (
    for (field <- 0 to size * size - 1) yield field
  )(collection.breakOut)

  // all locations on the board for jumping around
  val locations: Vector[Location] =
    for (field <- possibleLocations)
      yield Location(field, this)

  // used to verify wall validity
  val possibleWallLocations: Vector[Location] = for {
    location <- locations
    if (!location.isBorder(South)) && (!location.isBorder(East))
  } yield location

  // possible walls on the board
  val possibleWalls: Vector[Wall] = for {
    location <- possibleWallLocations
    orientation <- List(Horizontal, Vertical)
  } yield Wall(location, orientation)

  def containsLocation(location: Int): Boolean =
    possibleLocations contains location
}

case class Board(
  boardType: BoardType,
  black: Location,
  white: Location,
  walls: Set[Wall] = Set()
) {
  def canMove(location: Location, direction: Direction): Boolean = {
    if (! location.neighbor(direction).isDefined)
      return false

    val locationsToCheck: Seq[Option[Location]] = direction match {
      case North => Seq(
        location.neighbor(North),
        location.neighbor(North).flatMap(l => l.neighbor(West)))
      case South => Seq(Some(location), location.neighbor(West))
      case East => Seq(Some(location), location.neighbor(North))
      case West => Seq(
        location.neighbor(West),
        location.neighbor(West).flatMap(l => l.neighbor(North)))
    }

    locationsToCheck.flatten
      .filter(l => l.allowsWallPlacement)
      .filter(l => walls contains Wall(l, direction.orientation.opposite))
      .length == 0
  }

  override def toString = {
    def getOptionalLocation(position: Int): Option[Location] = {
      Option(position)
        .filter(pos => boardType.containsLocation(pos))
        .map(pos => Location(pos, boardType))
    }

    def canGo(location: Option[Location], direction: Direction): Boolean = {
      location
        .filter(l => !l.isBorder(direction) && !canMove(l, direction))
        .map(l => false).getOrElse(true)
    }

    val oddLines = (-1 to boardType.size - 1)
      .map(row => (0 to boardType.size - 1).map(column => {
        val optLoc = getOptionalLocation(row*boardType.size + column)
        if (canGo(optLoc, South)) "+   "
        else "+---"
      }).mkString + "+")

    val evenLines = (0 to boardType.size - 1)
      .map(row => (0 to boardType.size - 1).map(column => {
        val optLoc = getOptionalLocation(row*boardType.size + column)
        val side =
          if (canGo(optLoc, West)) " "
          else "|"

        val pawn = if (optLoc == Some(black)) "x"
          else if (optLoc == Some(white)) "o"
          else " "

        side + " " + pawn + " "
      }).mkString.replaceAll("""\s+$""",""))

    List(oddLines, evenLines).flatMap(_.zipWithIndex)
      .sortBy(_._2).map(_._1).mkString("\n") + "\n"
  }
}
