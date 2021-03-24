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
}
