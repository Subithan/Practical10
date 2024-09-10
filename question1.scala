def createRational(n: Int, d: Int): (Int, Int) = {
  require(d > 0, "Denominator must be greater than 0")
  (n, d)
}

def addRational(r1: (Int, Int), r2: (Int, Int)): (Int, Int) = {
  val (n1, d1) = r1
  val (n2, d2) = r2
  (n1 * d2 + n2 * d1, d1 * d2)
}

def negRational(r: (Int, Int)): (Int, Int) = {
  val (n, d) = r
  (-n, d)
}

def rationalToString(r: (Int, Int)): String = {
  val (n, d) = r
  s"$n/$d"
}

def main(args: Array[String]): Unit = {
  val obj = createRational(1, 2)
  val objj = createRational(4, 5)

  println(rationalToString(negRational(obj)))

  val q = addRational(obj, objj)
  println(rationalToString(q))
}
