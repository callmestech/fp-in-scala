package com.callmestech.exercises.chapter3

object Chapter3 extends App {
  val list = List(1, 2, 3, 4)
  val e: List[Int] = Nil
  println(list.foldL(1)(_ + _))
  println(list.foldL(1)(_ * _))
  println(list.reduce(_ + _))
  println(x)
}
