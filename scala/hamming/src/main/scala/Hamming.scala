object Hamming {
  def distance(s1: String, s2: String): Option[Int] =
    if (s1.size != s2.size) None
    else Some(s1.zip(s2).count(_ != _))
}
