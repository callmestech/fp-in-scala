package com.callmestech.exercises.chapter4

/**  Named Or instead of either to avoid collisions with built-in type
  */
sealed trait Or[+E, +A] {
  import Or._

  /** Exercise 4.6
    *
    * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
    */

  def map[B](f: A => B): E Or B = this match {
    case l @ Left(_) => l
    case Right(v)    => Right(f(v))
  }
  def flatMap[EE >: E, B](f: A => EE Or B): EE Or B = this match {
    case l @ Left(_)  => l
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => EE Or B): EE Or B = this match {
    case r @ Right(_) => r
    case _            => b
  }

  def map2[EE >: E, B, C](b: EE Or B)(f: (A, B) => C): EE Or C = {
    for {
      a <- this
      b <- b
    } yield f(a, b)
  }
}

object Or {
  final case class Left[+E](value: E) extends Or[E, Nothing]
  final case class Right[+A](value: A) extends Or[Nothing, A]

  def Try[A](a: => A): Or[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  /** Exercise 4.7
    *
    * Implement sequence and traverse for Either.
    * These should return the first error that’s encountered, if there is one.
    *  {{{def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]]}}}
    *  {{{def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]}}}
    */
  def sequence[E, A](es: List[Or[E, A]]): E Or List[A] =
    traverse(es)(identity)

  def traverse[E, A, B](as: List[A])(f: A => Or[E, B]): Or[E, List[B]] =
    as.foldRight(Right(List.empty[B]): Or[E, List[B]])((h, t) => f(h).map2(t)(_ :: _))
}
