object Etl {
  def transform(scoreMap: Map[Int, Seq[String]]): Map[String, Int] = {
    scoreMap
      .flatMap((score, letters) => letters.map(_.toLowerCase -> score))
  }
}