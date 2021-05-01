package com.callmestech.exercises

import java.util.concurrent.{ExecutorService, Future, TimeUnit}
import scala.concurrent.TimeoutException

package object chapter7 {

//  def sum(xs: IndexedSeq[Int]): Par[Int] = {
//    if (xs.size <= 1) Par.unit(xs.headOption.getOrElse(0))
//    else {
//      val (l, r) = xs.splitAt(xs.size / 2)
//      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
//    }
//  }

  trait Par[A] {}

  object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(par: Par[A]): Future[A] = par(s)

    def fork[A](a: => Par[A]): Par[A] = es => {
      es.submit(() => a(es).get()) // new Callable converted to SAM
    }

    private case class UnitFuture[T](t: T) extends Future[T] {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
      override def isCancelled: Boolean = false
      override def isDone: Boolean = true
      override def get(): T = t

      override def get(timeout: Long, unit: TimeUnit): T = {
        val now = System.nanoTime()

        if (unit.toNanos(timeout) - now > 0) t
        else throw new TimeoutException("Time is out")
      }
    }

    /** Exercise 7.3
     *
     * Hard: Fix the implementation of map2 so that it respects the contract of timeouts on Future.
     * */
    private case class Map2Future[A, B, C](a: Future[A],
                                           b: Future[B],
                                           fn: (A, B) => C) extends Future[C] {
      @volatile private var cache: Option[C] = None

      override def cancel(mayInterruptIfRunning: Boolean): Boolean =
        a.cancel(mayInterruptIfRunning) || b.cancel(mayInterruptIfRunning)

      override def isCancelled: Boolean = a.isCancelled || b.isCancelled

      override def isDone: Boolean = cache.isDefined

      override def get(): C = compute(Long.MaxValue)

      override def get(timeout: Long, unit: TimeUnit): C =
        compute(unit.toNanos(timeout))

      private def compute(timeoutInNanos: Long): C = cache match {
        case Some(value) => value
        case _ =>
          val start = System.nanoTime()
          val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
          val stop = System.nanoTime() - start
          val aTime = stop - start
          val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)

          val ret = fn(ar, br)
          cache = Some(ret)
          ret
      }
    }

    /** Exercise 7.1
      *
      * <b>map2</b> is a new higher-order function for combining the result of two parallel computations.
      * What is its signature? Give the most general signature possible (don’t assume it works only for Int).
      */
    def map2[A, B, C](parA: Par[A], parB: Par[B])
                     (combine: (A, B) => C): Par[C] = es => {
      val (fa, fb) = parA(es) -> parB(es)
      Map2Future(fa, fb, combine)
    }

    /** Exercise 7.4
     *
     * This API already enables a rich set of operations.
     * Here’s a simple example: using lazyUnit,
     * write a function to convert any function A => B to one that evaluates its result asynchronously.
     * */
    def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def sortPar(par: Par[List[Int]]): Par[List[Int]] =
      map(par)(_.sorted)

    /** Exercise 7.5
     *
     * Hard: Write this function, called sequence. No additional primitives are required.
     * Do not call run.
     * */
    def sequence[A](ps: List[Par[A]]): Par[List[A]] = es =>
     ps.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_ :: _))(es)

    def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
      case Nil => unit(Nil)
      case ::(head, t) => map2(head, fork(sequenceRight(t)))(_ :: _)
    }

    def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
      if (ps.isEmpty) unit(Nil)
      else if (ps.length == 1) map(ps.head)(a => Vector(a))
      else {
        val (l, r) = ps.splitAt(ps.size / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
      val fbs: List[Par[B]] = ps.map(asyncF(f))
      sequence(fbs)
    }
  }
}
