package com.callmestech.exercises

import com.callmestech.exercises.chapter3.List.sum

import scala.annotation.tailrec

package object chapter3 {

  sealed trait List[+A] extends Product with Serializable {

    def ::[B >: A](b: B): Cons[B] = Cons(b, this)

    def reverse: List[A] = {
      @tailrec
      def loop(as: List[A], acc: List[A]): List[A] = as match {
        case Nil => acc
        case Cons(h, t) => loop(t, h :: acc)
      }

      loop(this, Nil)
    }

    def foldL[B](z: B)(f: (B, A) => B): B = {
      @tailrec
      def loop(z: B)(f: (B, A) => B, rem: List[A]): B = rem match {
        case Nil => z
        case Cons(h, t) => loop(f(z, h))(f, t)
      }
      loop(z)(f, this)
    }

    def reduce[B >: A](f: (B, B) => B): B = this match {
      case Nil => throw new UnsupportedOperationException("Empty.reduce")
      case Cons(h, t) =>
        @tailrec
        def loop(f: (B, B) => B, acc: B, rem: List[B]): B = rem match {
          case Nil => acc
          case Cons(h, t) => loop(f, f(acc, h), t)
        }

        loop(f, h, t)
    }


    /**  Exercise 3.2
     *
     * Implement the function tail for removing the first element of a List.
     * Note that the function takes constant time. What are different choices you could make
     * in your implementation if the List is Nil? We’ll return to this question in the next chapter. **/
    def tail: List[A] = this match {
      case Nil => this
      case Cons(_, t) => t
    }

    /** Exercise 3.3
     *
     * Using the same idea, implement the function setHead
     * for replacing the first element of a List with a different value.
    **/
    def setHead[B >: A](b: B): List[B] = this match {
      case Nil => Cons(b, Nil)
      case Cons(_, t) => Cons(b, t)
    }

    /** Exercise 3.4
     *
     * Generalize tail to the function drop, which removes the first n elements from a list.
     * Note that this function takes time proportional only to the number of elements
     * being dropped—we don’t need to make a copy of the entire List.
     **/
    def drop(n: Int): List[A] =
      if (n <= 0) this
      else tail.drop(n - 1)

    /** Exercise 3.5
     *
     * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
     **/
    def dropWhile(p: A => Boolean): List[A] = this match {
      case Nil => this
      case Cons(h, t) => if (p(h)) t.dropWhile(p) else this
    }

    /** Exercise 3.6
     *
     * Not everything works out so nicely. Implement a function, init,
     * that returns a List consisting of all but the last element of a List. So, given List(1,2,3,4),
     * init will return List(1,2,3).
     * Why can’t this function be implemented in constant time like tail?
     *
     * My implementation has O(N^2^) complexity. It's horrible. How could I do better?
    **/
    def init: List[A] = {
      @tailrec
      def loop(as: List[A], acc: List[A]): List[A] = as match {
        case Nil          => acc
        case Cons(_, Nil) => acc
        case Cons(h, t)   => loop(t, Cons(h, acc))
      }

      loop(this, Nil).reverse
    }
  }

  final case object Nil extends List[Nothing]
  final case class Cons[+A](h: A, t: List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(h, t) => h + sum(t)
    }
  }
//  What will be the result of the following match expression? /
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}
