package com.callmestech.exercises

import java.util.concurrent.{ExecutorService, Future, TimeUnit}
import scala.concurrent.TimeoutException

object Par {
  type Par[A] = ExecutorService => Future[A]

  def sum(xs: IndexedSeq[Int]): Par[Int] =
    Par.parFold(xs.toList)(0)(_ + _)

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
    */
  private case class Map2Future[A, B, C](
      a: Future[A],
      b: Future[B],
      fn: (A, B) => C
  ) extends Future[C] {
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
  def map2[A, B, C](parA: Par[A], parB: Par[B])(combine: (A, B) => C): Par[C] =
    es => {
      val (fa, fb) = parA(es) -> parB(es)
      Map2Future(fa, fb, combine)
    }

  /** Exercise 7.4
    *
    * This API already enables a rich set of operations.
    * Here’s a simple example: using lazyUnit,
    * write a function to convert any function A => B to one that evaluates its result asynchronously.
    */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(par: Par[List[Int]]): Par[List[Int]] =
    map(par)(_.sorted)

  /** Exercise 7.5
    *
    * Hard: Write this function, called sequence. No additional primitives are required.
    * Do not call run.
    */
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = es =>
    ps.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_ :: _))(es)

  def sequenceRight[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil         => unit(Nil)
    case ::(head, t) => map2(head, fork(sequenceRight(t)))(_ :: _)
  }

  def sequenceBalanced[A](ps: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (ps.isEmpty) unit(Vector.empty)
    else if (ps.length == 1) map(ps.head)(a => Vector(a))
    else {
      val (l, r) = ps.splitAt(ps.size / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /** Exercise 7.6
    *
    * Implement parFilter, which filters elements of a list in parallel.
    */
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars = as.map(asyncF(a => if (f(a)) List(a) else List.empty))
    map(sequence(pars))(_.flatten)
  }

  def parFilterViaParMap[A](as: List[A])(f: A => Boolean): Par[List[A]] =
    map(parMap(as)(a => if (f(a)) List(a) else List.empty))(_.flatten)

  def parFold[A](as: List[A])(default: A)(combine: (A, A) => A): Par[A] = {
    if (as.isEmpty) Par.unit(default)
    else {
      val (p1, p2) = as.splitAt(as.size / 2)
      Par.map2(
        fork(parFold(p1)(default)(combine)),
        fork(parFold(p2)(default)(combine))
      )(combine)
    }
  }

  /** Write a function that takes a list of paragraphs
    * (a List[String]) and returns the total
    * number of words across all paragraphs, in parallel.
    * Generalize this function as much as possible.
    */
  def paragraphs(xs: List[String]): Par[Int] =
    xs.foldRight(unit(0))((s, acc) =>
      map2(lazyUnit(s.count(_ == ' ')), acc)(_ + _)
    )

  def paragraphsViaParFoldMap(xs: List[String]): Par[Int] =
    parFoldMap(xs)(0)(_.count(_ == ' '))(_ + _)

  def parFoldMap[A, B](as: List[A])(z: B)(f: A => B)(op: (B, B) => B): Par[B] =
    as.foldRight(unit(z))((a, parB) => fork(map2(lazyUnit(f(a)), parB)(op)))

  /** Implement map3, map4, and map5, in terms of map2.
    */
  def map3[A, B, C, D](parA: Par[A], parB: Par[B], parC: Par[C])(
      combine: (A, B, C) => D
  ): Par[D] =
    map2(
      map2(parA, parB)((a, b) => combine(a, b, _)),
      parC
    )(_(_))

  def map4[A, B, C, D, E](
      parA: Par[A],
      parB: Par[B],
      parC: Par[C],
      parD: Par[D]
  )(combine: (A, B, C, D) => E): Par[E] =
    map2(
      map2(
        map2(parA, parB)((a, b) => combine(a, b, _, _)),
        parC
      )((cdToE, c) => cdToE(c, _)),
      parD
    )((f, d) => f(d))

  def map5[A, B, C, D, E, F](parA: Par[A],
                             parB: Par[B],
                             parC: Par[C],
                             parD: Par[D],
                             parE: Par[E])
                            (combine: (A, B, C, D, E) => F): Par[F] =
    map2(
      map2(
        map2(
          map2(parA, parB)((a, b) => combine(a, b, _, _, _)),
          parC
        )((f1, c) => f1(c, _, _)),
        parD
      )((f2, d) => f2(d, _)),
      parE
    )((f3, e) => f3(e))

  /** Exercise 7.7
   *
   * Hard: Given map(y)(id) == y, it’s a free theorem
   * that map(map(y)(g))(f) == map(y)(f compose g).
   * (This is sometimes called map fusion, and it can be used as an optimization—rather
   * than spawning a separate parallel computation to compute
   * the second mapping, we can fold it into the first mapping.)
   * Can you prove it? You may want to read
   * the paper “Theorems for Free!” (http://mng.bz/Z9f1) to better understand the “trick” of free theorems.
   * */

  //given map(y)(id) = y
  //prove that map(map(y)(g))(f) == map(y)(f compose g)
//  ----------------------------
//  assume that g = id
//  map(map(y)(id))(f) = map(y)(f compose id)
//  simplify both sides
//  map(y)(id) = y; f compose id = f; substitute to the equations -
//  map(y)(f) = map(y)(f)
}
