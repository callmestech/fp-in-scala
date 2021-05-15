package com.callmestech.exercises.chapter8

trait Prop {
  type FailedCase = String
  type SuccessCount = Int

  def check: Either[(FailedCase, SuccessCount), SuccessCount]

  /** Exercise 8.3
   *
   * Assuming the following representation of Prop, implement && as a method of Prop.
   * */
//  def &&(p: Prop): Prop = new Prop {
//    override def check: Boolean = Prop.this.check && p.check
//  }
}
