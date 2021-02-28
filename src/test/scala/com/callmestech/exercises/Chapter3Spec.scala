package com.callmestech.exercises

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import Chapter3._
import com.callmestech.exercises.Chapter3.List.{addOne, doubles2Strings, lengthViaFoldL, productViaFoldL, sumViaFoldL}
import org.scalatest.matchers.should.Matchers
import org.scalacheck.Gen

class Chapter3Spec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  private val PosSeqGen = Gen.containerOf[Seq, Int](Gen.posNum[Int])
  private val DoubleSeqGen = Gen.containerOf[Seq, Double](Gen.double)
  private val NonEmptyPosSeqGen = Gen.nonEmptyContainerOf[Seq, Int](Gen.posNum[Int])

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

  property("init should return all elems of list but last") {
    forAll(NonEmptyPosSeqGen) { xs =>
      val list = List(xs: _*)

      list.init shouldEqual List(xs.init: _*)
      list.init2 shouldEqual List(xs.init: _*)
    }
  }

  property("++ should add elems of one list in the tail of another list") {
    forAll(PosSeqGen, PosSeqGen) { (xs: Seq[Int], ys: Seq[Int]) =>
      val list1 = List(xs: _*)
      val list2 = List(ys: _*)

      list1 ++ list2 shouldEqual List(xs ++ ys: _*)
    }
  }


  property("foldL should correctly calculate sum / product of list") {
    forAll(PosSeqGen) { (xs: Seq[Int]) =>
      val list = List(xs: _*)

      list.foldL(0)(_ + _) shouldEqual xs.sum
      list.foldL(1)(_ * _) shouldEqual xs.product
    }
  }

  property("reduce should throws") {
    forAll(PosSeqGen) { xs =>
      val list = List(xs: _*)

      if (list.isEmpty) assertThrows[UnsupportedOperationException](list.reduce(_ + _))
      else list.reduce(_ + _) shouldEqual xs.sum
    }
  }

  property("foldR should correctly calculate sum / product of list") {
    forAll(PosSeqGen) { (xs: Seq[Int]) =>
      val list = List(xs: _*)

      list.foldR(0)(_ + _) shouldEqual xs.sum
      list.foldR(1)(_ * _) shouldEqual xs.product
    }
  }

  property("length should correctly calculate length of the list") {
    forAll(PosSeqGen) { (xs: Seq[Int]) =>
      val list = List(xs: _*)

      list.length shouldEqual xs.length
    }
  }

  property("exercise 3.11") {
    forAll(PosSeqGen) { (xs: Seq[Int]) =>
      val list = List(xs: _*)

      lengthViaFoldL(list) shouldEqual xs.length
      sumViaFoldL(list) shouldEqual xs.sum
      productViaFoldL(list) shouldEqual xs.product
    }
  }

  property("flatten should concatenate all of the lists into one list") {
    forAll(PosSeqGen, PosSeqGen) { (xs, ys) =>
      val list1 = List(xs: _*)
      val list2 = List(ys: _*)

      val flat = List.flatten(List(list1, list2))
      flat shouldEqual List(Seq(xs, ys).flatten :_*)
    }
  }

  property("addOne should add 1 to each elem of a list") {
    forAll(PosSeqGen) { xs =>
      val list = List(xs: _*)

      addOne(list) shouldEqual List(xs.map(_ + 1) :_*)
    }
  }

  property("doubles2strings should transform each elem of list into string") {
    forAll(DoubleSeqGen) { xs =>
      val list = List(xs: _*)

      doubles2Strings(list) shouldEqual List(xs.map(_.toString) :_*)
    }
  }

  property("filter should correctly filter out elems that doesn't match to the predicate") {
    forAll(PosSeqGen) { xs =>
      val list = List(xs: _*)

      list.filter(_ > 2) shouldEqual List(xs.filter(_ > 2): _*)
      list.filter2(_ > 2) shouldEqual List(xs.filter(_ > 2): _*)
    }
  }
}
