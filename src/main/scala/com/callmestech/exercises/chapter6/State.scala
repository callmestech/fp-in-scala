package com.callmestech.exercises.chapter6

final case class State[S, +A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s1 =>
    val (a, s2) = run(s1)
    f(a).run(s2)
  }

  def map2[B, C](other: State[S, B])
                (f: (A, B) => C): State[S, C] =
    flatMap(a => other.map(b => f(a, b)))

}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](xs: List[State[S, A]]): State[S, List[A]] =
    xs.foldRight(unit[S, List[A]](List.empty))((s, acc) => s.map2(acc)(_ :: _))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def get[S]: State[S, S] = State(s => s -> s)

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
}