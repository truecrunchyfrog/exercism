import scala.collection.mutable

class School {
  type DB = Map[Int, Seq[String]]

  val data = mutable.Map[Int, Seq[String]]()

  def add(name: String, g: Int) =
    data(g) = data.getOrElseUpdate(g, {Nil}) :+ name

  def db: DB = data.toMap

  def grade(g: Int): Seq[String] = data.getOrElse(g, Seq())

  def sorted: DB =
    db.mapValues(_.sorted).toMap
}