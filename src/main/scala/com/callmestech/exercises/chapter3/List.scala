package com.callmestech.exercises.chapter3

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

sealed trait List[+A] extends Product with Serializable {

  import List._

  def ::[B >: A](b: B): Cons[B] = Cons(b, this)

  def isEmpty: Boolean = this == Nil

  /** Exercise 3.10
    */
  def foldL[B](z: B)(f: (B, A) => B): B = {
    @tailrec
    def loop(rem: List[A], z: B)(f: (B, A) => B): B = rem match {
      case Nil        => z
      case Cons(h, t) => loop(t, f(z, h))(f)
    }

    loop(this, z)(f)
  }

  /** Exercise 3.12
    *
    * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
    * See if you can write it using a fold.
    */
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
    */
  def foldRViaFoldL[B](z: B)(f: (A, B) => B): B =
    this.reverse.foldL(z)((b, a) => f(a, b))

  def foldRViaFoldL2[B](z: B)(f: (A, B) => B): B =
    this.reverse.foldL((b: B) => b)((g, a) => b => g(f(a, b)))(z)

  /** Exercise 3.14
    *
    * Implement append in terms of either foldLeft or foldRight.
    */
  def ++[A1 >: A](a1s: List[A1]): List[A1] =
    foldR(a1s)(Cons(_, _))

  def foldR[B](z: B)(f: (A, B) => B): B = this match {
    case Nil        => z
    case Cons(h, t) => f(h, t.foldR(z)(f))
  }

  def reduce[B >: A](f: (B, B) => B): B = this match {
    case Nil => throw new UnsupportedOperationException("Empty.reduce")
    case Cons(h, t) =>
      @tailrec
      def loop(f: (B, B) => B, acc: B, rem: List[B]): B = rem match {
        case Nil        => acc
        case Cons(h, t) => loop(f, f(acc, h), t)
      }

      loop(f, h, t)
  }

  /** Exercise 3.2
    *
    * Implement the function tail for removing the first element of a List.
    * Note that the function takes constant time. What are different choices you could make
    * in your implementation if the List is Nil? We’ll return to this question in the next chapter. *
    */
  def tail: List[A] = this match {
    case Nil        => this
    case Cons(_, t) => t
  }

  /** Exercise 3.3
    *
    * Using the same idea, implement the function setHead
    * for replacing the first element of a List with a different value.
    */
  def setHead[B >: A](b: B): List[B] = this match {
    case Nil        => Cons(b, Nil)
    case Cons(_, t) => Cons(b, t)
  }

  /** Exercise 3.4
    *
    * Generalize tail to the function drop, which removes the first n elements from a list.
    * Note that this function takes time proportional only to the number of elements
    * being dropped—we don’t need to make a copy of the entire List.
    */
  def drop(n: Int): List[A] =
    if (n <= 0) this
    else tail.drop(n - 1)

  /** Exercise 3.5
    *
    * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
    */
  def dropWhile(p: A => Boolean): List[A] = this match {
    case Nil        => this
    case Cons(h, t) => if (p(h)) t.dropWhile(p) else this
  }

  /** Exercise 3.6
    *
    * Not everything works out so nicely. Implement a function, init,
    * that returns a List consisting of all but the last element of a List. So, given List(1,2,3,4),
    * init will return List(1,2,3).
    * Why can’t this function be implemented in constant time like tail?
    */
  def init: List[A] = {
    def loop(as: List[A]): List[A] = as match {
      case Nil          => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, loop(t))
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
    */
  def length: Int = foldR(0)((_, acc) => acc + 1)

  /** Exercise 3.18
    *
    * Write a function map that generalizes modifying each element in a list
    * while maintaining the structure of the list.
    */
  def map[B](f: A => B): List[B] =
    foldRViaFoldL(Nil: List[B])((a, bs) => Cons(f(a), bs))

  /** Exercise 3.19
    *
    * Write a function filter that removes elements from a list unless they satisfy a given predicate.
    * Use it to remove all odd numbers from a List[Int].
    */
  def filter(p: A => Boolean): List[A] = {
    val buffer = new ListBuffer[A]

    @tailrec
    def go(xs: List[A], buffer: ListBuffer[A], p: A => Boolean): List[A] =
      xs match {
        case Nil        => List(buffer.toList: _*)
        case Cons(h, t) => if (p(h)) go(t, buffer += h, p) else go(t, buffer, p)
      }

    go(this, buffer, p)
  }

  def filter2(p: A => Boolean): List[A] =
    foldRViaFoldL(Nil: List[A])((a, as) => if (p(a)) Cons(a, as) else as)

  /** Exercise 3.20
    *
    * Write a function flatMap that works like map
    * except that the function given will return a list instead of a single result,
    * and that list should be inserted into the final resulting list. Here is its signature:
    * def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]
    * For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
    */

  def flatMap[B](f: A => List[B]): List[B] =
    foldR(Nil: List[B])((a, bs) => f(a) ++ bs)

  def flatMap2[B](f: A => List[B]): List[B] =
    List.flatten(map(f))

  /** Exercise 3.21
    *
    * Use flatMap to implement filter.
    */
  def filterViaFlatmap(p: A => Boolean): List[A] =
    flatMap(a => if (p(a)) List(a) else Nil)

  def zipWith[B, C](other: List[B])(combine: (A, B) => C): List[C] =
    (this, other) match {
      case (Nil, _) | (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) =>
        Cons(combine(x, y), xs.zipWith(ys)(combine))
    }

  /** Exercise 3.24
    *
    * Hard: As an example, implement hasSubsequence for checking whether a List contains another List as a subsequence.
    * For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
    * You may have some difficulty finding a concise purely functional implementation that is also efficient.
    * That’s okay. Implement the function however comes most naturally.
    * We’ll return to this implementation in chapter 5 and hopefully improve on it.
    * Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
    * fixme: look at the implementation in answers and compare it
    */
  def hasSubsequence[A1 >: A](sub: List[A1]): Boolean = {
    val size = sub.length

    def go(
        sup: List[A1],
        sub: List[A1],
        matched: Int,
        isStreak: Boolean
    ): Boolean = {
      (sup, sub) match {
        case (Nil, _) | (_, Nil) => matched == size
        case (Cons(x, xs), Cons(y, ys)) =>
          if (x == y && (isStreak || matched == 0))
            go(xs, ys, matched + 1, true)
          else go(xs, sub, matched, false)
      }
    }

    go(this, sub, 0, false)
  }
}

object List {

  final case object Nil extends List[Nothing]

  final case class Cons[+A](h: A, t: List[A]) extends List[A]

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil        => 0
    case Cons(h, t) => h + sum(t)
  }

  /** Exercise 3.7
    *
    * Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
    * Why or why not? Consider how any short-circuiting might work if you call foldRight with a large list.
    * This is a deeper question that we’ll return to in chapter 5.
    */
  def productViaFoldR(as: List[Int]): Int =
    as.foldR(1)((a, acc) =>
      if (a == 0) return 0 else a * acc
    ) // seems that isn't possible

  /** Exercise 3.8
    *
    * See what happens when you pass Nil and Cons themselves to foldRight,
    * like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
    * What do you think this says about the relationship between foldRight and the data constructors of List?
    */
  //      [1, 2, 3].foldR(Nil: List[Int])(Cons(_, _))
  //      Cons(1, [2, 3].foldR(Nil)(f))
  //      Cons(1, Cons(2, [3].foldR(Nil)(f)))
  //      Cons(1, Cons(2, Cons(3, Nil.foldR(Nil)(f))))
  def foo(as: List[Int]) =
    as.foldR(Nil: List[Int])(Cons(_, _))

  /** Exercise 3.11
    *
    * Write sum, product, and a function to compute the length of a list using foldLeft
    */
  def sumViaFoldL(xs: List[Int]): Int = xs.foldL(0)(_ + _)

  def productViaFoldL(xs: List[Int]): Int = xs.foldL(1)(_ * _)

  def lengthViaFoldL(xs: List[Int]): Int = xs.foldL(0)((acc, _) => acc + 1)

  /** Exercise 3.15
    *
    * Hard: Write a function that concatenates a list of lists into a single list.
    * Its runtime should be linear in the total length of all lists.
    * Try to use functions we have already defined.
    */
  def flatten[A](ass: List[List[A]]): List[A] =
    ass.foldL(Nil: List[A])(_ ++ _)

  /** Exercise 3.16
    *
    * Write a function that transforms a list of integers by adding 1 to each element.
    * (Reminder: this should be a pure function that returns a new List!)
    */
  def addOne(xs: List[Int]): List[Int] =
    xs.foldR(Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  def doubles2Strings(xs: List[Double]): List[String] =
    xs.foldRViaFoldL(Nil: List[String])((a, acc) => Cons(a.toString, acc))

  /** Exercise 3.22
    *
    * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
    * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
    */

  def combineElemsOfLists(as: List[Int], bs: List[Int]): List[Int] =
    (as, bs) match {
      case (_, Nil) => as
      case (Nil, _) => bs
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Cons(h1 + h2, combineElemsOfLists(t1, t2))
    }
}
