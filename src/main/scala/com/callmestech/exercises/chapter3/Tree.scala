package com.callmestech.exercises.chapter3

sealed trait Tree[+A] extends Product with Serializable {
  import Tree._

  /** Exercise 3.25
   *
   *  Write a function size that counts the number of nodes (leaves and branches) in a tree.
   * */
  def size: Int = this match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size
  }
}

object Tree {
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]


  /** Exercise 3.26
   *
   * Write a function maximum that returns the maximum element in a Tree[Int].
   * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x andy.)
   * */

  def max(t: Tree[Int]): Int = {
    def go(rem: Tree[Int], lastMax: Int): Int = {
      rem match {
        case Leaf(value) => lastMax.max(value)
        case Branch(l, r) => max(l).max(max(r))
      }
    }
    go(t, Int.MinValue)
  }
}


