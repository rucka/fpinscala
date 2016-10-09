package ch3
import scala.annotation.tailrec

object Exercise28 {
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    case Leaf(v) => Leaf(f(v))
  }
}

import Exercise28._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/Tree.scala
  :load src/main/scala/fpinscala/ch3/Exercise28.scala
  map(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))(_ + 1)
  map(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))(_ * 2)
*/
