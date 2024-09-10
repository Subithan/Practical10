def createRational(n: Int, d: Int): (Int, Int) = {
  require(d > 0, "Denominator must be greater than 0")
  (n, d)
}

def addRational(r1: (Int, Int), r2: (Int, Int)): (Int, Int) = {
  val (n1, d1) = r1
  val (n2, d2) = r2
  ((n1 * d2) + (n2 * d1), d1 * d2)
}

def negRational(r: (Int, Int)): (Int, Int) = {
  val (n, d) = r
  (-n, d)
}

def subtractRational(r1: (Int, Int), r2: (Int, Int)): (Int, Int) = {
  addRational(r1, negRational(r2))
}

def rationalToString(r: (Int, Int)): String = {
  val (n, d) = r
  s"$n/$d"
}

def main(args: Array[String]): Unit = {
  val x = createRational(3, 4)
  val y = createRational(5, 8)
  val z = createRational(2, 7)
  val result = subtractRational(subtractRational(x, y), z)

  println(rationalToString(result))
}
