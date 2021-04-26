package com.callmestech.exercises

package object chapter7 {

  def sum(xs: IndexedSeq[Int]): Par[Int] = {
    if (xs.size <= 1) Par.unit(xs.headOption.getOrElse(0))
    else {
      val (l, r) = xs.splitAt(xs.size / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }
  }

  trait Par[A] {}

  object Par {
    def unit[A](a: A): Par[A] = ???

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](par: Par[A]): A = ???

    def fork[A](a: => Par[A]): Par[A] = ???

    /** Exercise 7.1
      *
      * <b>map2</b> is a new higher-order function for combining the result of two parallel computations.
      * What is its signature? Give the most general signature possible (don’t assume it works only for Int).
      */
    def map2[A, B, C](parA: Par[A], parB: Par[B])(
        combine: (A, B) => C
    ): Par[C] = ???
  }
}
