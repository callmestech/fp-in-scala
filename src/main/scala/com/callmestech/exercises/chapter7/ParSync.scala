package com.callmestech.exercises.chapter7

import java.util.concurrent.{ExecutorService, TimeUnit, Future => JFuture}
import scala.concurrent.TimeoutException

object ParSync {
  type ParSync[A] = ExecutorService => JFuture[A]

  def run[A](es: ExecutorService)(parSync: ParSync[A]): JFuture[A] =
    parSync(es)

  def unit[A](a: A): ParSync[A] = _ => UnitFuture(a)

  def fork[A](a: => ParSync[A]): ParSync[A] = es => es.submit(() => a(es).get())

  def lazyUnit[A](a: => A): ParSync[A] = fork(unit(a))

  /** Exercise 7.1
    *
    * <b>map2</b> is a new higher-order function for combining the result of two parallel computations.
    * What is its signature? Give the most general signature possible (don’t assume it works only for Int).
    */
  def map2[A, B, C](parA: ParSync[A], parB: ParSync[B])(
      combine: (A, B) => C
  ): ParSync[C] =
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
  def asyncF[A, B](f: A => B): A => ParSync[B] = a => lazyUnit(f(a))

  def map[A, B](pa: ParSync[A])(f: A => B): ParSync[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(par: ParSync[List[Int]]): ParSync[List[Int]] =
    map(par)(_.sorted)

  /** Exercise 7.5
    *
    * Hard: Write this function, called sequence. No additional primitives are required.
    * Do not call run.
    */
  def sequence[A](ps: List[ParSync[A]]): ParSync[List[A]] = es =>
    ps.foldRight(unit(List.empty[A]))((a, b) => map2(a, b)(_ :: _))(es)

  def sequenceRight[A](ps: List[ParSync[A]]): ParSync[List[A]] = ps match {
    case Nil         => unit(Nil)
    case ::(head, t) => map2(head, fork(sequenceRight(t)))(_ :: _)
  }

  def sequenceBalanced[A](ps: IndexedSeq[ParSync[A]]): ParSync[IndexedSeq[A]] =
    fork {
      if (ps.isEmpty) unit(Vector.empty)
      else if (ps.length == 1) map(ps.head)(a => Vector(a))
      else {
        val (l, r) = ps.splitAt(ps.size / 2)
        map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
      }
    }

  def parMap[A, B](ps: List[A])(f: A => B): ParSync[List[B]] = fork {
    val fbs: List[ParSync[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  /** Exercise 7.6
    *
    * Implement parFilter, which filters elements of a list in parallel.
    */
  def parFilter[A](as: List[A])(f: A => Boolean): ParSync[List[A]] = {
    val pars = as.map(asyncF(a => if (f(a)) List(a) else List.empty))
    map(sequence(pars))(_.flatten)
  }

  def parFilterViaParMap[A](as: List[A])(f: A => Boolean): ParSync[List[A]] =
    map(parMap(as)(a => if (f(a)) List(a) else List.empty))(_.flatten)

  def parFold[A](as: List[A])(default: A)(combine: (A, A) => A): ParSync[A] = {
    if (as.isEmpty) ParSync.unit(default)
    else {
      val (p1, p2) = as.splitAt(as.size / 2)
      ParSync.map2(
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
  def paragraphs(xs: List[String]): ParSync[Int] =
    xs.foldRight(unit(0))((s, acc) =>
      map2(lazyUnit(s.count(_ == ' ')), acc)(_ + _)
    )

  def paragraphsViaParFoldMap(xs: List[String]): ParSync[Int] =
    parFoldMap(xs)(0)(_.count(_ == ' '))(_ + _)

  def parFoldMap[A, B](as: List[A])(z: B)(f: A => B)(
      op: (B, B) => B
  ): ParSync[B] =
    as.foldRight(unit(z))((a, parB) => fork(map2(lazyUnit(f(a)), parB)(op)))

  /** Implement map3, map4, and map5, in terms of map2.
    */
  def map3[A, B, C, D](parA: ParSync[A], parB: ParSync[B], parC: ParSync[C])(
      combine: (A, B, C) => D
  ): ParSync[D] =
    map2(
      map2(parA, parB)((a, b) => combine(a, b, _)),
      parC
    )(_(_))

  def map4[A, B, C, D, E](
      parA: ParSync[A],
      parB: ParSync[B],
      parC: ParSync[C],
      parD: ParSync[D]
  )(combine: (A, B, C, D) => E): ParSync[E] =
    map2(
      map2(
        map2(parA, parB)((a, b) => combine(a, b, _, _)),
        parC
      )((cdToE, c) => cdToE(c, _)),
      parD
    )((f, d) => f(d))

  def map5[A, B, C, D, E, F](
      parA: ParSync[A],
      parB: ParSync[B],
      parC: ParSync[C],
      parD: ParSync[D],
      parE: ParSync[E]
  )(combine: (A, B, C, D, E) => F): ParSync[F] =
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
    */

  //given map(y)(id) = y
  //prove that map(map(y)(g))(f) == map(y)(f compose g)
  //  ----------------------------
  //  assume that g = id
  //  map(map(y)(id))(f) = map(y)(f compose id)
  //  simplify both sides
  //  map(y)(id) = y; f compose id = f; substitute to the equations -
  //  map(y)(f) = map(y)(f)
  /** Exercise 7.11
    *
    * Implement choiceN and then choice in terms of choiceN.
    */
  def choiceN[A](n: ParSync[Int])(choices: List[ParSync[A]]): ParSync[A] = es =>
    {
      val ind = run(es)(n).get

      run(es)(choices(ind))
    }

  def choiceViaChoiceN[A](
      cond: ParSync[Boolean]
  )(onTrue: ParSync[A], onFalse: ParSync[A]): ParSync[A] =
    choiceN(map(cond)(b => if (b) 0 else 1))(List(onTrue, onFalse))

  /** Exercise 7.12
    */
  def choiceMap[K, V](key: ParSync[K])(
      choices: Map[K, ParSync[V]]
  ): ParSync[V] = es => run(es)(map(key)(k => run(es)(choices(k)).get()))

  /** Exercise 7.13
    *
    * Implement this new primitive chooser, and then use it to implement choice and choiceN.
    */
  def flatMap[A, B](pa: ParSync[A])(choices: A => ParSync[B]): ParSync[B] =
    es => {
      val a = run(es)(pa).get()
      run(es)(choices(a))
    }

  def choiceViaFlatMap[A](
      cond: ParSync[Boolean]
  )(onTrue: ParSync[A], onFalse: ParSync[A]): ParSync[A] =
    flatMap(cond)(b => if (b) onTrue else onFalse)

  def choiceNViaFlatMap[A](n: ParSync[Int])(
      choices: List[ParSync[A]]
  ): ParSync[A] =
    flatMap(n)(idx => choices(idx))

  /** Exercise 7.14
    *
    * Implement join. Can you see how to implement flatMap using join?
    * And can you implement join using flatMap?
    */
  def join[A](a: ParSync[ParSync[A]]): ParSync[A] =
    flatMap(a)(identity)

  def flatMapViaJoin[A, B](pa: ParSync[A])(
      choices: A => ParSync[B]
  ): ParSync[B] =
    join(map(pa)(a => choices(a)))

  private case class UnitFuture[T](t: T) extends JFuture[T] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
    override def isCancelled: Boolean                            = false
    override def isDone: Boolean                                 = true
    override def get(): T                                        = t

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
      a: JFuture[A],
      b: JFuture[B],
      fn: (A, B) => C
  ) extends JFuture[C] {
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
        val ar    = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop  = System.nanoTime() - start
        val aTime = stop - start
        val br    = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)

        val ret = fn(ar, br)
        cache = Some(ret)
        ret
    }
  }
}
