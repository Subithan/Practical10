def countLetterOccurrences(words: List[String]): Int = {
  val lengths = words.map(_.length)
  val totalLength = lengths.reduce(_ + _)
  totalLength
}

def main(args: Array[String]): Unit = {
  val words = List("apple", "banana", "cherry", "date")
  val totalOccurrences = countLetterOccurrences(words)
  println(s"Total count of letter occurrences: $totalOccurrences")
}
