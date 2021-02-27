package com.callmestech.exercises

import com.callmestech.exercises.Chapter3.List.sum

import scala.annotation.tailrec

object Chapter3 {

  sealed trait List[+A] extends Product with Serializable {

    def ::[B >: A](b: B): Cons[B] = Cons(b, this)

    def isEmpty: Boolean = this == Nil

    /** Exercise 3.10
     *
     * */
    def foldL[B](z: B)(f: (B, A) => B): B = {
      @tailrec
      def loop(rem: List[A], z: B)(f: (B, A) => B): B = rem match {
        case Nil => z
        case Cons(h, t) => loop(t, f(z, h))(f)
      }

      loop(this, z)(f)
    }

    /** Exercise 3.12
     *
     * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
     * See if you can write it using a fold.
     * */
    def reverse: List[A] =
      foldL(Nil: List[A])((acc, a) => Cons(a, acc))

    def reverseViaFoldR: List[A] =
      foldR(Nil: List[A])((a, acc) => Cons(a, acc))

    /** Exercise 3.13
     *
     * Hard: Can you write foldLeft in terms of foldRight?
     * How about the other way around?
     * Implementing foldRight via foldLeft is useful because it lets us implement foldRight tail-recursively,
     * which means it works even for large lists without overflow- ing the stack.
     * */
    def foldRViaFoldL[B](z: B)(f: (A, B) => B): B =
      this.reverse.foldL(z)((b, a) => f(a, b))

    def foldRViaFoldL2[B](z: B)(f: (A, B) => B): B =
      this.reverse.foldL((b: B) => b)((g, a) => b => g(f(a, b)))(z)

    /** Exercise 3.14
     *
     * Implement append in terms of either foldLeft or foldRight.
     * */
    def ++[A1 >: A](a1s: List[A1]): List[A1] =
      foldR(a1s)(Cons(_, _))

    def foldR[B](z: B)(f: (A, B) => B): B = this match {
      case Nil => z
      case Cons(h, t) => f(h, t.foldR(z)(f))
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


    /** Exercise 3.2
     *
     * Implement the function tail for removing the first element of a List.
     * Note that the function takes constant time. What are different choices you could make
     * in your implementation if the List is Nil? We’ll return to this question in the next chapter. * */
    def tail: List[A] = this match {
      case Nil => this
      case Cons(_, t) => t
    }

    /** Exercise 3.3
     *
     * Using the same idea, implement the function setHead
     * for replacing the first element of a List with a different value.
     * */
    def setHead[B >: A](b: B): List[B] = this match {
      case Nil => Cons(b, Nil)
      case Cons(_, t) => Cons(b, t)
    }

    /** Exercise 3.4
     *
     * Generalize tail to the function drop, which removes the first n elements from a list.
     * Note that this function takes time proportional only to the number of elements
     * being dropped—we don’t need to make a copy of the entire List.
     * */
    def drop(n: Int): List[A] =
      if (n <= 0) this
      else tail.drop(n - 1)

    /** Exercise 3.5
     *
     * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
     * */
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
     * */
    def init: List[A] = {
      def loop(as: List[A]): List[A] = as match {
        case Nil => sys.error("init on empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, loop(t))
      }

      loop(this)
    }

    def init2: List[A] = {
      @tailrec
      def loop(as: List[A], acc: List[A]): List[A] = as match {
        case Nil          => sys.error("init on empty list")
        case Cons(_, Nil) => acc
        case Cons(h, t)   => loop(t, Cons(h, acc))
      }
      loop(this, Nil).reverse
    }

    /** Exercise 3.9
     *
     * Compute the length of a list using foldRight.
     * */
    def length: Int = foldR(0)((_, acc) => acc + 1)

    override def toString: String = {
      val str: String = s"List("
      //todo : now it works incorrectly
      @tailrec
      def loop(xs: List[A], acc: String): String = xs match {
        case Nil => s"$acc)"
        case Cons(h, Nil) => s"$acc$h)"
        case Cons(h, t) => loop(t, s"$h, ")
      }

      s"$str${loop(this, "")}"
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

    /** Exercise 3.7
     *
     * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
     * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
     * This is a deeper question that we’ll return to in chapter 5. */
    def productViaFoldR(as: List[Int]): Int =
      as.foldR(1)((a, acc) => if (a == 0) return 0 else a * acc) // seems that isn't possible

    /** Exercise 3.8
     *
     * See what happens when you pass Nil and Cons themselves to foldRight,
     * like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
     * What do you think this says about the relationship between foldRight and the data constructors of List?
     * */
    //      [1, 2, 3].foldR(Nil: List[Int])(Cons(_, _))
    //      Cons(1, [2, 3].foldR(Nil)(f))
    //      Cons(1, Cons(2, [3].foldR(Nil)(f)))
    //      Cons(1, Cons(2, Cons(3, Nil.foldR(Nil)(f))))
    def foo(as: List[Int]) =
      as.foldR(Nil: List[Int])(Cons(_, _))

    /** Exercise 3.11
     *
     * Write sum, product, and a function to compute the length of a list using foldLeft
     * */
    def sumViaFoldL(xs: List[Int]): Int = xs.foldL(0)(_ + _)

    def productViaFoldL(xs: List[Int]): Int = xs.foldL(1)(_ * _)

    def lengthViaFoldL(xs: List[Int]): Int = xs.foldL(0)((acc, _) => acc + 1)
  }

  //  What will be the result of the following match expression? /
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}
