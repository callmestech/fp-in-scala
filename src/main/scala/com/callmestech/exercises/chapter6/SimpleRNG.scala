package com.callmestech.exercises.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
    val nextRng = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    n -> nextRng
  }
}

object SimpleRNG {

  /** Exercise 6.1
   *
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
   * Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart.
   * {{{def nonNegativeInt(rng: RNG): (Int, RNG)}}}
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (newInt, newRng) = rng.nextInt

    (if (newInt < 0) -(newInt + 1) else newInt, newRng)
  }

  /** Exercise 6.2
   *
   * Write a function to generate a Double between 0 and 1, not including 1. Note:
   * You can use Int.MaxValue to obtain the maximum positive integer value,
   * and you can use x.toDouble to convert an x: Int to a Double.
   * {{{def double(rng: RNG): (Double, RNG)}}}
   * */
  def double(rng: RNG): (Double, RNG) = {
    val (i1, r1) = nonNegativeInt(rng)

    (i1 / (Int.MaxValue.toDouble + 1), r1)
  }

  /** Exercise 6.3
   *
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a (Double, Double, Double) 3-tuple.
   * You should be able to reuse the functions you’ve already written.
   * {{{def intDouble(rng: RNG): ((Int,Double), RNG)}}}
   * {{{def doubleInt(rng: RNG): ((Double,Int), RNG)}}}
   * {{{def double3(rng: RNG): ((Double,Double,Double), RNG)}}}
   * */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  /** Exercise 6.4
   *
   * Write a function to generate a list of random integers.
   * {{{def ints(count: Int)(rng: RNG): (List[Int], RNG)}}}
   * */
   def ints(count: Int)(rng: RNG): (List[Int], RNG) = {

     def go(i: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
       if (i > 0) {
         val (i1, r) = acc._2.nextInt
         go(i - 1, (i1 :: acc._1, r))
       } else acc
     }
     go(count, (List.empty, rng))
   }
}
