package com.callmestech.exercises.chapter8

import com.callmestech.exercises.chapter6.RNG
import com.callmestech.exercises.chapter8.Prop.{FailedCase, SuccessCount, TestCases}
import com.callmestech.exercises.chapter8.Result.{Falsified, Passed}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, rng) =>
    run(max, rng) match {
      case Passed => p.run(max, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop { (max, rng) =>
    run(max, rng) match {
      case Falsified(_, _) => p.run(max, rng)
      case x => x
    }
  }
}

object Prop {
  type FailedCase = String
  type TestCases = Int
  type SuccessCount = Int
  type Result = Option[(FailedCase, SuccessCount)]

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomLazyList(gen)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map {
        case (a, i) => try {
          if (f(a)) Passed
          else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified)
      .getOrElse(Passed)
  }

  def randomLazyList[A](gen: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(gen.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace: ${e.getStackTrace.mkString("\n")}"
}

sealed trait Result {
  def isFalsified: Boolean
}

object Result {

  final case object Passed extends Result {
    override def isFalsified: Boolean = false
  }

  final case class Falsified(failure: FailedCase,
                             successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }
}
