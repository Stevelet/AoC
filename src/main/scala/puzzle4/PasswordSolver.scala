package puzzle4

object PasswordSolver {
  def verifyValid(password: Int): Boolean = {
    val string = password.toString

    var doubleFound = false
    var max = Integer.parseInt(string(0).toString)
    for (i <- 1 until string.length) {
      if (Integer.parseInt(string(i).toString) < max) return false

      if (Integer.parseInt(string(i - 1).toString) == Integer.parseInt(string(i).toString)) {
        val beforeSame = if (i - 2 >= 0) Integer.parseInt(string(i - 2).toString) == Integer.parseInt(string(i).toString) else false
        val afterSame = if (i + 1 < string.length) Integer.parseInt(string(i + 1).toString) == Integer.parseInt(string(i).toString) else false
        if (!beforeSame && !afterSame) doubleFound = true
      }

      max = Math.max(Integer.parseInt(string(i).toString), max)
    }
    doubleFound
  }

  def main(args: Array[String]): Unit = {
    println((123257 to 647015).count(verifyValid))
  }
}
