class DNA(dna: String):
    def nucleotideCounts: Either[String, Map[Char, Int]] =
        Map('A' -> 0, 'C' -> 0, 'G' -> 0, 'T' -> 0) ++
        dna.groupBy(c => c).map((nuc, values) => nuc -> values.size) match
            case counts if counts.size == 4 => Right(counts)
            case _ => Left("error")