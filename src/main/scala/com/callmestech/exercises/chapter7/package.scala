package com.callmestech.exercises

package object chapter7 {

  def sum(xs: IndexedSeq[Int]): Int = {
    if (xs.size <= 1) xs.headOption.getOrElse(0)
    else {
      val (l, r) = xs.splitAt(xs.size / 2)
      sum(l) + sum(r)
    }
  }
}
