package com.callmestech.exercises.chapter7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, TimeUnit}
import scala.concurrent.TimeoutException

sealed trait Future[A] {
  private[chapter7] def apply(k: A => Unit): Unit
}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ =>
    new Future[A] {
      def apply(k: A => Unit): Unit = k(a)
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(par: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    par(s) { a =>
      ref.set(a)
      latch.countDown()
    }
    latch.await()
    ref.get()
  }


  def fork[A](a: => Par[A]): Par[A] = es => {
    new Future[A] {
      def apply(k: A => Unit): Unit = eval(es)(a(es)(k))
    }
  }

  def eval[A](es: ExecutorService)(r: => Unit): Unit = {
    es.submit(new Callable[Unit] {
      def call: Unit = r
    })
  }

  def delay[A](parA: => Par[A]): Par[A] =
    es => parA(es)

  def map2[A, B, C](parA: Par[A], parB: Par[B])
                   (combine: (A, B) => C): Par[C] = ??? // there should be non-blocking implementation

  /** Exercise 7.4
    *
    * This API already enables a rich set of operations.
    * Here’s a simple example: using lazyUnit,
    * write a function to convert any function A => B to one that evaluates its result asynchronously.
    */
  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))
}
