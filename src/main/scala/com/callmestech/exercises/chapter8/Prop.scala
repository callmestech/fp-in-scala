package com.callmestech.exercises.chapter8

trait Prop {
  def check: Boolean

  /** Exercise 8.3
   *
   * Assuming the following representation of Prop, implement && as a method of Prop.
   * */
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = Prop.this.check && p.check
  }
}
