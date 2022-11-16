package com.callmestech.exercises.chapter4

import scala.annotation.tailrec

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
    */

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

  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def isEmpty: Boolean = this match {
    case Some(_) => false
    case None    => true
  }

  /*
  def lift[A1 >: A, B >: A](f: A1 => B): MyOption[A1] => MyOption[B] =
    _.map(f)

  val optAbs = lift(math.abs) */

  /** Exercise 4.3
    *
    * Write a generic function map2 that combines two Option values using a binary function.
    * If either Option value is None, then the return value is too. Here is its signature:
    * def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
    */
  def map2[B, C](other: MyOption[B])(f: (A, B) => C): MyOption[C] =
    flatMap(a => other.map(b => f(a, b)))
}

object MyOption {

  final case class Some[+A](get: A) extends MyOption[A]
  case object None extends MyOption[Nothing]

  /** Exercise 4.2
    *
    * Implement the variance function in terms of flatMap.
    * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
    * See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
    */
  def variance(xs: Seq[Double]): MyOption[Double] = {
    def mean(xs: Seq[Double]): MyOption[Double] = {
      if (xs.isEmpty) None else Some(xs.sum / xs.size)
    }

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /** Exercise 4.3
    */
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(
      f: (A, B) => C
  ): MyOption[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  /** Exercise 4.4
    *
    * Write a function sequence that combines a list of Options into one Option containing
    * a list of all the Some values in the original list.
    * If the original list contains None even once, the result of the function should be None;
    * otherwise the result should be Some with a list of all the values.
    */
  def sequence[A](xs: List[MyOption[A]]): MyOption[List[A]] = {

    @tailrec
    def go(xs: List[MyOption[A]], acc: MyOption[List[A]]): MyOption[List[A]] =
      xs match {
        case Nil => acc
        case ::(head, next) =>
          go(next, head.flatMap(a => acc.map(as => a :: as)))
      }

    if (xs.isEmpty) None else go(xs, Some(Nil))
  }

  def sequence2[A](xs: List[MyOption[A]]): MyOption[List[A]] = xs match {
    case Nil            => Some(Nil)
    case ::(head, next) => head.flatMap(h => sequence2(next).map(h :: _))
  }

  def sequenceViaMap2[A](xs: List[MyOption[A]]): MyOption[List[A]] =
    xs.foldRight(Some(Nil): MyOption[List[A]])((acc, a) => acc.map2(a)(_ :: _))

  def sequenceViaTraverse[A](xs: List[MyOption[A]]): MyOption[List[A]] =
    traverse(xs)(identity)

  /** Exercise 4.5
    */
  def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] = {
    @tailrec
    def go(xs: List[A], acc: MyOption[List[B]]): MyOption[List[B]] = xs match {
      case Nil            => acc
      case ::(head, next) => go(next, acc.flatMap(xxs => f(head).map(_ :: xxs)))
    }

    go(as, Some(Nil))
  }

  def traverseViaFoldR[A, B](
      as: List[A]
  )(f: A => MyOption[B]): MyOption[List[B]] =
    as.foldRight(Some(Nil): MyOption[List[B]])((a, acc) => map2(f(a), acc)(_ :: _))

  def traverse_2[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    as match {
      case Nil            => Some(Nil)
      case ::(head, next) => map2(f(head), traverse_2(next)(f))(_ :: _)
    }
}
