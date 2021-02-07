package com.callmestech.exercises

import com.callmestech.exercises.chapter3.List.sum

import scala.annotation.tailrec

package object chapter3 {

  sealed trait List[+A] extends Product with Serializable {

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
      case Cons(h, t) => {
        def loop(f: (B, B) => B, acc: B, rem: List[B]): B = rem match {
          case Nil => acc
          case Cons(h, t) => loop(f, f(acc, h), t)
        }
        loop(f, h, t)
      }
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
