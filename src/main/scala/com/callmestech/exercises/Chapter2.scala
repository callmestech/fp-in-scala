package com.callmestech.exercises

import scala.annotation.tailrec

object Chapter2 extends App {

  /** exercise 2.1
   *
   * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
   * The first two Fibonacci numbers are 0 and 1.
   * The nth number is always the sum of the previous two—the sequence begins 0, 1, 1, 2, 3, 5.
   * Your definition should use a local tail-recursive function.
   * */
  def fib(n: Int): Int = {
    @tailrec
    def gogo(n: Int, prev: Int, cur: Int): Int = {
      if (n <= 0) cur
      else {
        println(s"n - $n, prev - $prev, cur - $cur")
        gogo(n - 1, prev = prev + cur, cur = prev)
      }
    }

    gogo(n, prev = 1, cur = 0)
  }

  /** exercise 2.2
   *
   * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function:
   * */
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length == 0 || as.length == 1) true
    else if (ordered(as(0), as(1))) isSorted(as.tail, ordered)
    else false
  }

  /** exercise 2.3
   *
   * Let’s look at another example, currying,9 which converts a function f of two arguments
   * into a function of one argument that partially applies f.
   * Here again there’s only one implementation that compiles. Write this implementation.
   * */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => f(a, _)

  /** exercise 2.4
   *
    * Implement uncurry, which reverses the transformation of curry.
    * Note that since => associates to the right, A => (B => C) can be written as A => B => C.
    * */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  /** exercise 2.5
   *
   * Implement the higher-order function that composes two functions.
   **/
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

  println(fib(20))
}
