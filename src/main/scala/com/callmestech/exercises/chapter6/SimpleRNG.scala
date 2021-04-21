package com.callmestech.exercises.chapter6

import scala.annotation.tailrec

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
  type Rand[+A] = State[RNG, A]
  type State[S, +A] = S => (A, S)


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

     @tailrec
     def go(i: Int, acc: (List[Int], RNG)): (List[Int], RNG) = {
       if (i > 0) {
         val (i1, r) = acc._2.nextInt
         go(i - 1, (i1 :: acc._1, r))
       } else acc
     }
     go(count, (List.empty, rng))
   }

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  /** Exercise 6.5
   *
   * Use map to reimplement double in a more elegant way. See exercise 6.2.
   * */
  def double2: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  /** Exercise 6.6
   *
   * Write the implementation of map2 based on the following signature.
   * This function takes two actions, ra and rb, and a function f for combining their results,
   * and returns a new action that combines them:
   * {{{def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]}}}
   * */
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng1) = ra(rng)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)(_ -> _)

  /** Exercise 6.7
   *
   * Hard: If you can combine two RNG transitions, you should be able to combine a whole list of them.
   * Implement sequence for combining a List of transitions into a single transition.
   * Use it to reimplement the ints function you wrote before. For the latter,
   * you can use the standard library function {{{List.fill(n)(x)}}} to make a list with x repeated n times.
   * */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {

    @tailrec
    def go(xs: List[Rand[A]], acc: (List[A], RNG)): (List[A], RNG) = xs match {
      case Nil    => acc
      case h :: t =>
        val (a, r) = h(acc._2)
        go(t, (a :: acc._1, r))
    }

    go(fs, (List.empty, rng))
  }

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) => map2(f, acc)(_ :: _))

  /** Exercise 6.8
   *
   * Implement flatMap, and then use it to implement nonNegativeLessThan.
   * */
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  /** Exercise 6.9
   *
   * Reimplement <b>map</b> and <b>map2</b> in terms of <b>flatMap</b>.
   * The fact that this is possible is what we’re referring to when we say that flatMap is more powerful than map and map2.
   * */
  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](r1: Rand[A], r2: Rand[B])
                             (f: (A, B) => C): Rand[C] =
    flatMap(r1)(a => map(r2)(b => f(a, b)))
}
