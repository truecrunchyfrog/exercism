object House:
    val parts = Vector(
        ("lay in", "house that Jack built"),
        ("ate", "malt"),
        ("killed", "rat"),
        ("worried", "cat"),
        ("tossed", "dog"),
        ("milked", "cow with the crumpled horn"),
        ("kissed", "maiden all forlorn"),
        ("married", "man all tattered and torn"),
        ("woke", "priest all shaven and shorn"),
        ("kept", "rooster that crowed in the morn"),
        ("belonged to", "farmer sowing his corn"),
        ("", "horse and the hound and the horn")
    )

    def formatTailPart(s: (String, String)): String =
        s"that ${s._1} the ${s._2}"

    def verse(n: Int): String =
        s"This is the ${parts(n)._2}" +
        (if (n > 0)
            " " + (n - 1 to 0 by - 1).map(i => formatTailPart(parts(i))).mkString(" ")
        else "") +
        "."

    def recite(start: Int, stop: Int): String =
        (start - 1 to stop - 1).map(verse).mkString("\n") + "\n\n"