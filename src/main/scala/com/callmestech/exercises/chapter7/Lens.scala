package com.callmestech.exercises.chapter7

final case class Lens[A, B](get: A => B, set: (A, B) => A)

object Lens {
  def compose[I, O, V](outer: Lens[O, I], inner: Lens[I, V]): Lens[O, V] =
    Lens(
      outer.get.andThen(inner.get),
      (o, v) => outer.set(o, inner.set(outer.get(o), v))
    )
}
