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
  def factorial(n: Int): BigInt =
    def fact(n: Int, acc: BigInt): BigInt =
      if n <= 1 then acc
      else fact(n - 1, acc * n)
    fact(n, 1)

  /** Calculate the combination of two given numbers
    * @param n
    *   the first number
    * @param k
    *   the second number
    * @return
    *   n choose k
    */
  def calculateCombination(n: Int, k: Int): BigInt = 
    factorial(n) / (factorial(k) * factorial(n - k))
end ClinksCalculator
