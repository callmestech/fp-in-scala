package com.callmestech.exercises.chapter8

import com.callmestech.exercises.chapter6.{RNG, State}

final case class Gen[A](sample: State[RNG, A]) {
  /** Exercise 8.6
   *
   * Implement flatMap, and then use it to implement
   * this more dynamic version of listOfN.
   * Put flatMap and listOfN in the Gen class.
   * */
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(this, i))
}

object Gen {

  /** Exercise 8.4
    *
    * Implement Gen.choose using this representation of Gen.
    * It should generate integers in the range start to stopExclusive.
    * Feel free to use functions you’ve already written.
    */
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(RNG.nonNegativeInt).map(i => start + i % (stopExclusive - start))
    )

  /** Exercise 8.5
   *
   * Let’s see what else we can implement using this representation of Gen.
   * Try implementing unit, boolean, and listOfN.
   * */
  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(rng => RNG.boolean(rng)))

  def listOfN[A](gen: Gen[A], n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(gen.sample)))

  def tuple(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    Gen(
      choose(start, stopExclusive)
        .sample
        .flatMap(i =>
          choose(start, stopExclusive).sample.map(j => i -> j)
        )
    )

  def string(size: Int): Gen[String] =
    Gen(
      listOfN(choose(97, 122), size) // a to z
        .sample
        .map(xs => xs.map(_.toChar).mkString)
    )

  /** Exercise 8.7
   *
   * Implement union, for combining two generators of the same type into one,
   * by pulling values from each generator with equal likelihood.
   * */
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  /** Exercise 8.8
   *
   * Implement weighted, a version of union that accepts
   * a weight for each Gen and generates values from each Gen with probability proportional to its weight.
   * */
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    /* The probability we should pull from `g1`. */
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(
      State(RNG.double)
        .flatMap(d =>
          if (d < g1Threshold) g1._1.sample else g2._1.sample
        )
    )
  }
}
