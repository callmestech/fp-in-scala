package com.callmestech.exercises.chapter3

object Chapter3 extends App {
  val list = List(1, 2, 3, 4)
  val e: List[Int] = Nil
  println(list.dropWhile(_ <= 3))
  println(list.init)
  println(list.reverse)
}
