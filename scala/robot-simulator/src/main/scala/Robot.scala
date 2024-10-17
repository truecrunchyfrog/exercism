enum Bearing(val delta: (Int, Int)):
    case North extends Bearing(0, 1)
    case South extends Bearing(0, -1)
    case East extends Bearing(1, 0)
    case West extends Bearing(-1, 0)

    def next(clockwise: Boolean): Bearing =
        val leftRight = this match
            case North => (East, West)
            case East => (South, North)
            case South => (West, East)
            case West => (North, South)
        if (clockwise) leftRight._1
        else leftRight._2

case class Robot(bearing: Bearing, coordinates: (Int, Int)):
    def turnRight: Robot = copy(bearing = bearing.next(true))
    def turnLeft: Robot = copy(bearing = bearing.next(false))

    def advance: Robot = copy(coordinates =
        (
            coordinates._1 + bearing.delta._1,
            coordinates._2 + bearing.delta._2
        )
    )

    def simulate(instructions: String): Robot = instructions.foldLeft(this)(
        (acc, i) => { i match
            case 'R' => acc.turnRight
            case 'L' => acc.turnLeft
            case 'A' => acc.advance
            case _ => throw Exception(s"invalid instruction: $i")
        })