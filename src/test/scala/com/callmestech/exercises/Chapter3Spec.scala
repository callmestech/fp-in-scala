package com.callmestech.exercises

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import Chapter3._
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Gen

class Chapter3Spec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  val PosSeqGen = Gen.containerOf[Seq, Int](Gen.posNum[Int])
  val ListGen: Gen[List[Int]] = for {
    xs <- Gen.containerOf[Seq, Int](Gen.posNum[Int])
  } yield List(xs:_*)

  property(":: should add elem to the front of list") {
    forAll { (i: Int) =>
      (i :: Nil).h == i shouldBe true
    }
  }

  property("correctly reverse elems of list") {
    forAll { (x1: Int, x2: Int) =>
      val list = List(x1, x2)

      list.reverse shouldEqual List(x2, x1)
      list.reverse.reverse shouldEqual list
    }
  }

  property("foldL should correctly calculate sum / product of list") {
    forAll(PosSeqGen) { (xs: Seq[Int]) =>
      val list = List(xs:_*)

      list.foldL(0)(_ + _) shouldEqual xs.sum
      list.foldL(1)(_ * _) shouldEqual xs.product
    }
  }

  property("reduce should throws") {
    forAll(PosSeqGen) { xs =>
      val list = List(xs:_*)

      if (list.isEmpty) assertThrows[UnsupportedOperationException](list.reduce(_ + _))
      else list.reduce(_ + _) shouldEqual xs.sum
    }
  }
}
