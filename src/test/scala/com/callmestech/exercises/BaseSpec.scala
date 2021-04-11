package com.callmestech.exercises

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

trait BaseSpec extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers
