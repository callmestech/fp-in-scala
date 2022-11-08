package com.callmestech.exercises.chapter4

/** Exercise 4.8
  */
sealed trait Validated[+E, +A] {

  import Validated._

  def map[B](f: A => B): Validated[E, B] = this match {
    case i @ Invalid(_) => i
    case Valid(value)   => Valid(f(value))
  }

  def findValid[E1 >: E, A1 >: A](
      that: Validated[E1, A1]
  )(implicit semigroup: Semigroup[E1]): Validated[E1, A1] = this match {
    case v1 @ Valid(_) => v1
    case Invalid(e1) =>
      that match {
        case v2 @ Valid(_) => v2
        case Invalid(e2)   => Invalid(semigroup.combine(e1, e2))
      }
  }

  def flatMap[B, E1 >: E](f: A => Validated[E1, B]): Validated[E1, B] =
    this match {
      case i @ Invalid(_) => i
      case Valid(v)       => f(v)
    }
}

object Validated {

  def invalid[E, A](es: Nel[E]): Validated[Nel[E], A] =
    Invalid(es)

  final case class Valid[+A](value: A) extends Validated[Nothing, A]

  final case class Invalid[+E](value: E) extends Validated[E, Nothing]

}

sealed trait Nel[+A]

final case class Node[+A](head: A, tail: Option[Nel[A]]) extends Nel[A]

sealed trait Semigroup[E] {
  def combine(e1: E, e2: E): E
}
