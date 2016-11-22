package ch6
import scala.annotation.tailrec
import RNG._

object Exercise10 {
  import State._
  case class State[S, +A] (run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))
    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def sequence[S, A](list: List[State[S, A]]): State[S, List[A]] =
      list.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
  }

  type Rand[A] = State[RNG, A]
  val int: Rand[Int] = State[RNG, Int](_.nextInt)
  def ints(count: Int): Rand[List[Int]] = State.sequence[RNG, Int](List.fill(count)(int))
}

import Exercise10._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch6/RNG.scala
:load src/main/scala/fpinscala/ch6/Exercise10.scala
*/
