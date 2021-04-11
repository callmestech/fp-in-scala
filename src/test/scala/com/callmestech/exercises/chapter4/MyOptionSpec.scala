package com.callmestech.exercises.chapter4

import com.callmestech.exercises.BaseSpec
import com.callmestech.exercises.chapter4.MyOption._
import org.scalacheck.Gen

class MyOptionSpec extends BaseSpec {

  private val MyOptionGen: Gen[MyOption[Int]] =
    for {
      num <- Gen.chooseNum(0, 50)
    } yield {
      if (num > 25) Some(num) else None
    }

  private val NonEmptyOption: Gen[MyOption[Int]] =
    Gen.posNum[Int].map(Some(_))

  private val ListMyOptionGen: Gen[List[MyOption[Int]]] = Gen.listOf(MyOptionGen)

  property("orElse should provide another Option in case of that is empty") {
    forAll(MyOptionGen suchThat(_.isEmpty)) { opt =>

      opt.orElse(Some(42)) shouldEqual Some(42)
    }
  }

  property("sequence should return none if at most one options is none") {
    forAll(ListMyOptionGen) { opts =>

      if (opts.forall(!_.isEmpty)) MyOption.sequence(opts) shouldEqual None
    }
  }

  property("sequence of options should return option of seq") {
    forAll(Gen.listOfN(10, NonEmptyOption)) { xs =>
      MyOption.sequence(xs).isEmpty shouldBe false
    }
  }
}
