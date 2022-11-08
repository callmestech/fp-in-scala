package com.callmestech.exercises.chapter5

import com.callmestech.exercises.BaseSpec
import org.scalacheck.Gen

class StreamSpec extends BaseSpec {

  val ListGen: Gen[List[Int]] = Gen.listOfN[Int](10, Gen.chooseNum(-100, 200))

  property("toList should return correct list") {
    forAll(ListGen) { xs =>
      val stream = Stream(xs: _*)

      stream.toList shouldEqual xs
    }
  }

  property("take should return first n elems of list") {
    forAll(ListGen) { xs =>
      val stream = Stream(xs: _*)

      stream.take(3).toList shouldEqual xs.take(3)
    }
  }

  property("drop should return list without n first elements") {
    forAll(ListGen) { xs =>
      val stream = Stream(xs: _*)

      stream.drop(3).toList shouldEqual xs.drop(3)
    }
  }

  property(
    "takeWhile should return all elems before first elem for which predicate doesn't holds"
  ) {
    forAll(ListGen) { xs =>
      val stream = Stream(xs: _*)

      stream.takeWhile(_ > 3).toList shouldEqual xs.takeWhile(_ > 3)
    }
  }
}
