package com.callmestech.exercises.chapter8

import com.callmestech.exercises.chapter6.{RNG, SimpleRNG}
import com.callmestech.exercises.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}
import com.callmestech.exercises.chapter8.Result.{Falsified, Passed}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop { (max, cases, rng) =>
    run(max, cases, rng) match {
      case Passed => p.run(max, cases, rng)
      case x      => x
    }
  }

  def ||(p: Prop): Prop = Prop { (max, cases, rng) =>
    run(max, cases, rng) match {
      case Falsified(_, _) => p.run(max, cases, rng)
      case x               => x
    }
  }
}

object Prop {
  type FailedCase   = String
  type TestCases    = Int
  type SuccessCount = Int
  type MaxSize      = Int

  def apply(f: (TestCases, RNG) => Result): Prop =
    Prop { (_, n, rng) => f(n, rng) }

  def run(
      prop: Prop,
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = SimpleRNG(System.nanoTime())
  ): Unit =
    prop.run(maxSize, testCases, rng) match {
      case Falsified(failure, successes) =>
        println(s"! Falsified after $successes passed tests:\n $failure")
      case Result.Passed =>
        println(s"+ OK passed $testCases tests.")
    }

  def forAll[A](gen: Int => Gen[A])(f: A => Boolean): Prop = Prop { (max, n, rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: LazyList[Prop] =
      LazyList.from(0).take(n.min(max) + 1).map(i => forAll(gen(i))(f))
    val prop: Prop =
      props
        .map(p =>
          Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }
        )
        .toList
        .reduce(_ && _)
    prop.run(max, n, rng)
  }

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomLazyList(gen)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) Passed
          else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(numberOfTestCases => g(numberOfTestCases))(f)

  val maxProp: Prop = forAll(Gen.listOf(Gen.smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  /** Exercice 8.14
    *
    *  Write a property to verify the behavior of List.sorted
    * (API docs link: http://mng.bz/ Pz86),
    *  which you can use to sort (among other things) a List[Int].
    * For instance, List(2,1,3).sorted is equal to List(1,2,3).
    */
  val sortedProp: Prop = forAll(Gen.listOf(Gen.smallInt)) { ns =>
    val sorted = ns.sorted

    // Sorted list is either empty, has one element,
    // or has no two consecutive elems a, b such that a > b
    val p1 = sorted.isEmpty || sorted.tail.isEmpty ||
      sorted
        .zip(sorted.drop(1))
        .forall { case (a, b) =>
          a <= b
        }

    // Also, the sorted list should have all the elements of the input list,
    val p2 = !ns.exists(!sorted.contains(_))

    // and it should have no elements not in the input list.
    val p3 = !sorted.exists(!ns.contains(_))

    p1 && p2 && p3
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

  final case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified: Boolean = true
  }
}
