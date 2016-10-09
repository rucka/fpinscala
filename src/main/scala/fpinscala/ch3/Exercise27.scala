package ch3
import scala.annotation.tailrec

object Exercise27 {
  def depth[A](tree: Tree[Int]): Int = tree match {
    case Branch(l, r) => 1 + (depth(l) max depth(r))
    case Leaf(v) => 0
  }
}

import Exercise27._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/Tree.scala
  :load src/main/scala/fpinscala/ch3/Exercise27.scala
  depth(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))
  depth(Branch(Leaf(3),Leaf(2)))
*/
