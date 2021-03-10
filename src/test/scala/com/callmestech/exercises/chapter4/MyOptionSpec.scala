package com.callmestech.exercises.chapter4

import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.callmestech.exercises.chapter4.MyOption._

class MyOptionSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  private val MyOptionGen: Gen[MyOption[Int]] =
    for {
      num <- Gen.chooseNum(0, 50)
    } yield {
      if (num > 25) Some(num) else None
    }

  property("orElse should provide another Option in case of that is empty") {
    forAll(MyOptionGen suchThat(_.isEmpty)) { opt =>

      opt.orElse(Some(42)) shouldEqual Some(42)
    }
  }
}
