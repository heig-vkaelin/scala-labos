package Utils

/** Contains the function necessary to calculate the number of *clinks* when n
  * people want to cheers.
  */
object ClinksCalculator:
  /** Calculate the factorial of a given number
    * @param n
    *   the number to compute
    * @return
    *   n!
    */
  // TODO - Part 1 Step 1
  def factorial(n: Int): BigInt = {
    def fact(n: Int, acc: BigInt): BigInt =
      if n <= 1 then acc
      else fact(n - 1, acc * n)
    fact(n, 1)
  }

  /** Calculate the combination of two given numbers
    * @param n
    *   the first number
    * @param k
    *   the second number
    * @return
    *   n choose k
    */
  // TODO - Part 1 Step 1
  def calculateCombination(n: Int, k: Int): Int = {
    val nFact = factorial(n)
    val kFact = factorial(k)
    val nMinusKFact = factorial(n - k)
    (nFact / (kFact * nMinusKFact)).toInt
  }
end ClinksCalculator
