object ScrabbleScore:
    val scoreMapping = Map(
        "aeioulnrst" -> 1,
        "dg" -> 2,
        "bcmp" -> 3,
        "fhvwy" -> 4,
        "k" -> 5,
        "jx" -> 8,
        "qz" -> 10,
    )

    def score(word: String): Int =
        word
            .flatMap(c => scoreMapping.find(_._1.contains(c.toLower)).map(_._2))
            .sum