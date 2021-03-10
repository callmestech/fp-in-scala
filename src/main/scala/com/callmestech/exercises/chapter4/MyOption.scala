package com.callmestech.exercises.chapter4

sealed trait MyOption[+A] {
  import MyOption._

  /** Exercise 4.1
   *
   * Implement all of the preceding functions on Option.
   * As you implement each function, try to think about what it means and in what situations you’d use it.
   * We’ll explore when to use each of these functions next. Here are a few hints for solving this exercise:
   *
   *  It’s fine to use pattern matching, though you should be able to implement
   * all the functions besides map and getOrElse without resorting to pattern matching.
   *
   *  For map and flatMap, the type signature should be enough to determine the implementation.
   *
   *  getOrElse returns the result inside the Some case of the Option,
   * or if the Option is None, returns the given default value.
   *
   *  orElse returns the first Option if it’s defined; otherwise, it returns the second Option.
   * */

  def map[B](f: A => B): MyOption[B] = this match {
    case Some(get) => Some(f(get))
    case None      => None
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None      => default
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): MyOption[A] = flatMap(a => if (f(a)) Some(a) else None)

  def isEmpty: Boolean = this match {
    case Some(_) => false
    case None    => true
  }
}

object MyOption {

  final case class Some[+A](get: A) extends MyOption[A]
  case object None extends MyOption[Nothing]

  /** Exercise 4.2
   *
   * Implement the variance function in terms of flatMap.
   * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
   * See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
   * */
  def variance(xs: Seq[Double]): MyOption[Double] = {
    def mean(xs: Seq[Double]): MyOption[Double] = {
      if (xs.isEmpty) None else Some(xs.sum / xs.size)
    }

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}
