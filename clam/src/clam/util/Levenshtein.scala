package clam.util

object Levenshtein:

  def distance(s1: String, s2: String): Int =
    val len1 = s1.length
    val len2 = s2.length
    val dp = Array.ofDim[Int](len1 + 1, len2 + 1)

    // Initialize the first row and column
    for i <- 0 to len1 do dp(i)(0) = i

    for j <- 0 to len2 do dp(0)(j) = j

    // Fill in the rest of the matrix
    for i <- 1 to len1 do
      for j <- 1 to len2 do
        val cost = if s1(i - 1) == s2(j - 1) then 0 else 1
        dp(i)(j) = Math.min(
          Math.min(dp(i - 1)(j) + 1, dp(i)(j - 1) + 1),
          dp(i - 1)(j - 1) + cost
        )

    // The bottom-right cell of the matrix contains the Levenshtein distance
    dp(len1)(len2)

  def closest(str: String, options: Iterable[String], max: Int = 2): List[String] =
    import collection.mutable.ListBuffer

    val candidates = ListBuffer.empty[String]
    val it = options.iterator

    while it.hasNext do
      val candidate = it.next()
      if distance(str, candidate) <= max then candidates += candidate

    candidates.sorted.result()
