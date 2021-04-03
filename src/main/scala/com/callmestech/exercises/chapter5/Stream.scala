package com.callmestech.exercises.chapter5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Cons(head, _) => Some(head())
    case Empty         => None
  }

  /** Exercise 5.1
   *
   * Write a function to convert a Stream to a List,
   * which will force its evaluation and let you look at it in the REPL.
   * You can convert to the regular List type in the standard library.
   * You can place this and other functions that operate on a Stream inside the Stream trait.
   *  {{{ def toList: List[A] }}}
   * */
  def toList: List[A] = this match {
    case Cons(head, tail) => head() :: tail().toList
    case Empty            => List.empty[A]
  }

  def toListTailRec: List[A] = {

    @tailrec
    def go(rem: Stream[A], acc: List[A]): List[A] = rem match {
      case Cons(head, tail) => go(tail(), head() :: acc)
      case Empty            => acc
    }

    go(this, Nil).reverse
  }

  /** Exercise 5.2
   *
   * Write the function take(n) for returning the first n elements of a Stream,
   * and drop(n) for skipping the first n elements of a Stream.
   * */
  def take(n: Int): Stream[A] = this match {
    case Cons(head, tail) if n > 1 => cons(head(), tail().take(n - 1))
    case Cons(h, _) if n == 1      => cons(h(), empty)
    case _                         => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, tail) if n > 0 => tail().drop(n - 1)
    case _                      => this
  }

  /** Exercise 5.3
   *
   * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
   * {{{def takeWhile(p: A => Boolean): Stream[A]}}}
   * */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(head, tail) if p(head()) => cons(head(), tail().takeWhile(p))
    case _                             => empty
  }

  def takeWhileTailRec(p: A => Boolean): Stream[A] = {

    @tailrec
    def go(rem: Stream[A], acc: Stream[A]): Stream[A] = rem match {
      case Cons(h, t) if p(h()) => go(t(), cons(h(), acc))
      case Empty                => acc
    }

    go(this, empty)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _          => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _          => z
  }

  def existsViaFoldR(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  /** Exercise 5.4
   *
   * Implement forAll, which checks that all elements in the Stream match a given predicate.
   * Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
   * {{{def forAll(p: A => Boolean): Boolean}}}
   * */
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  /** Exercise 5.5
   *
   * Use foldRight to implement takeWhile.
   * */
  def takeWhileViaFoldR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  /** Exercise 5.6
   *
   * Hard: Implement headOption using foldRight.
   * */
  def headOptionViaFoldR: Option[A] =
    foldRight(Option.empty[A])((a, _) => Some(a))

  /** Exercise 5.7
   *
   * Implement map, filter, append, and flatMap using foldRight.
   * The append method should be non-strict in its argument.
   * */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[A1 >: A](a: => Stream[A1]): Stream[A1] =
    foldRight(a)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  /** Exercise 5.8
   *
   * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
   *  {{{def constant[A](a: A): Stream[A]}}}
   * */
  def constant[A1 >: A](a: A1): Stream[A1] =
    cons(a, constant(a))

  /** Exercise 5.13
   *
   * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
   * The zipAll function should continue the traversal as long as either stream has
   * more elements it uses Option to indicate whether each stream has been exhausted.
   * {{{def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])]}}}
   * */
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(head, tail) => Some((f(head()), tail()))
      case Empty            => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, _), 1)          => Some((h(), (empty, 0)))
      case (Cons(h, _), i) if i > 1 => Some((h(), (empty, i - 1)))
      case _                        => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None
    }

  def zipWith[B, C](bs: Stream[B])
                   (f: (A, B) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(a, as), Cons(b, bs)) => Some((f(a(), b()), (as(), bs())))
      case _                          => None
    }

  def zip[B](bs: Stream[B]): Stream[(A, B)] =
    zipWith(bs)((_, _))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](bs: Stream[B])
                      (f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
      case (Cons(a, as), Empty)         => Some(f(Some(a()), Option.empty[B]) -> (as() -> empty[B]))
      case (Empty, Cons(b, bs))         => Some(f(Option.empty[A], Some(b())) -> (empty[A] -> bs()))
      case _                            => None
    }

  /** Exercise 5.14
   *
   * Hard: Implement startsWith using functions you’ve written.
   * It should check if one Stream is a prefix of another.
   * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
   * {{{ def startsWith[A](s: Stream[A]): Boolean }}}
   * */
  def startsWith(s: Stream[A]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll {
      case (h1, h2) => h1 == h2
    }
}

object Stream {

  final case class Cons[A](head: () => A, tail: () => Stream[A]) extends Stream[A]

  final case object Empty extends Stream[Nothing]

  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val hd = h
    lazy val tail = t

    Cons(() => hd, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  /** Exercise 5.9
   *
   * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.
   * {{{def from(n: Int): Stream[Int]}}}
   * */
  def from(i: Int): Stream[Int] =
    cons(i, from(i + 1))

  /** Exercise 5.10
   *
   * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
   * */
  val fibs: Stream[Int] = {
    def next(prev: Int, cur: Int): Stream[Int] = {
      cons(prev, next(cur, prev + cur))
    }

    next(0, 1)
  }

  /** Exercise 5.11
   *
   * Write a more general stream-building function called unfold.
   * It takes an initial state, and a function for producing both
   * the next state and the next value in the generated stream.
   * {{{def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]}}}
   * */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A]) { case (a, s) => cons(a, unfold(s)(f)) }

  /** Exercise 5.12
   *
   * Write fibs, from, constant, and ones in terms of unfold.
   * */
  val fibsViaUnfold: Stream[Int] =
    unfold((0, 1)) { case (prev, cur) => Some((prev, (cur, prev + cur))) }

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(i => Some((i, i + 1)))

  def constant(n: Int): Stream[Int] =
    unfold(n)(i => Some((i, i)))

  val ones: Stream[Int] =
    unfold(1)(_ => Some((1, 1)))
}
