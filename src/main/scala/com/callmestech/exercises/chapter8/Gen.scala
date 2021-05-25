package com.callmestech.exercises.chapter8

import com.callmestech.exercises.chapter6.{RNG, State}

final case class Gen[+A](sample: State[RNG, A]) {
  /** Exercise 8.6
   *
   * Implement flatMap, and then use it to implement
   * this more dynamic version of listOfN.
   * Put flatMap and listOfN in the Gen class.
   * */
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](gb: Gen[B])
                (f: (A, B) => C): Gen[C] =
    Gen(sample.map2(gb.sample)(f))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(this, i))

  /** Exercise 8.10
   *
   * Implement helper functions for converting Gen to SGen.
   * You can add this as a method on Gen.
   * */
  def unsized: SGen[A] = SGen(_ => this)

  def **[B](gb: Gen[B]): Gen[(A, B)] =
    map2(gb)(_ -> _)
}

case class SGen[+A](forSize: Int => Gen[A]) {
/** Exercise 8.11
 *
 * Not surprisingly, SGen at a minimum supports many of the
 * same operations as Gen, and the implementations are rather mechanical.
 * Define some convenience functions on SGen that simply
 * delegate to the corresponding functions on Gen.5
 * */
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(f(_).forSize(n)))

  def **[B](gb: SGen[B]): SGen[(A, B)] =
    SGen(n => apply(n) ** gb(n))
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

  /** Exercise 8.12
   *
   * Implement a listOf combinator that does not accept an explicit size.
   * It should return an SGen instead of a Gen.
   * The implementation should generate lists of the requested size.
   * */
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(listOfN(g, _))

  /** Exercise 8.13
   *
   * Define listOf1 for generating nonempty lists,
   * and then update your specification of max to use this generator.
   * */
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(g, 1.max(n)))
}
