object Bob:
  def response(statement: String): String =
    // Contains uppercase letter and doesn't contain lowercase letter = yelling.
    val yelling = statement.exists(_.isUpper) && !statement.exists(_.isLower)
    // Ends with ? = question.
    val question = statement.trim.endsWith("?")

    if (question && yelling) "Calm down, I know what I'm doing!"
    else if (question) "Sure."
    else if (yelling) "Whoa, chill out!"
    else if (statement.trim.isEmpty) "Fine. Be that way!"
    else "Whatever."