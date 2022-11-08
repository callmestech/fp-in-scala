package com.callmestech.exercises.chapter3

sealed trait Tree[+A] extends Product with Serializable {
  import Tree._

  //fixme: Tests should be added

  /** Exercise 3.25
    *
    * Write a function size that counts the number of nodes (leaves and branches) in a tree.
    */
  def size: Int = this match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + l.size + r.size
  }

  def sizeViaFold: Int =
    fold(_ => 1)((a1, a2) => 1 + a1 + a2)

  /** Exercise 3.27
    *
    * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    */
  def depth: Int = this match {
    case Leaf(_)      => 1
    case Branch(l, r) => l.depth.max(r.depth)
  }

  def depthViaFold: Int =
    fold(_ => 0)((a1, a2) => 1 + (a1 max a2))

  /** Exercise 3.28
    *
    * Write a function map, analogous to the method of the same name on List,
    * that modifies each element in a tree with a given function.
    */
  def map[B](f: A => B): Tree[B] = this match {
    case Leaf(value)  => Leaf(f(value))
    case Branch(l, r) => Branch(l.map(f), r.map(f))
  }

  def mapViaFold[B](f: A => B): Tree[B] =
    fold(a => Leaf(f(a)): Tree[B])((b1, b2) => Branch(b1, b2))

  /** Exercise 3.29
    *
    * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
    * Reimplement them in terms of this more general function.
    * Can you draw an analogy between this fold function and the left and right folds for List?
    */
  def fold[B](f: A => B)(combine: (B, B) => B): B = this match {
    case Leaf(value)  => f(value)
    case Branch(l, r) => combine(l.fold(f)(combine), r.fold(f)(combine))
  }
}

object Tree {
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  /** Exercise 3.26
    *
    * Write a function maximum that returns the maximum element in a Tree[Int].
    * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x andy.)
    */

  def max(t: Tree[Int]): Int = {
    def go(rem: Tree[Int], lastMax: Int): Int = {
      rem match {
        case Leaf(value)  => lastMax.max(value)
        case Branch(l, r) => max(l).max(max(r))
      }
    }
    go(t, Int.MinValue)
  }

  def maxViaFold(t: Tree[Int]): Int =
    t.fold(identity)(_ max _)
}
