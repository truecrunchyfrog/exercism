private case class Queen(x: Int, y: Int)

object Queen:
    def create(x: Int, y: Int): Option[Queen] =
        (x, y) match
            case _ if x < 0 || y < 0 => None
            case _ if x > 7 || y > 7 => None
            case _ => Some(Queen(x, y))

object QueenAttack:
    def canAttack(q1: Queen, q2: Queen): Boolean =
        // Horizontal
        q1.x == q2.x ||
        // Vertical
        q1.y == q2.y ||
        // Diagonal
        math.abs(q1.x - q2.x) == math.abs(q1.y - q2.y)