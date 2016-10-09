package ch3
import scala.annotation.tailrec

object Exercise25 {
  def size[A](tree: Tree[A]): Int = tree match {
    case Branch(l, r) => 1 + size(l) + size(r)
    case Leaf(_) => 1
  }
}

import Exercise25._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/Tree.scala
  :load src/main/scala/fpinscala/ch3/Exercise25.scala
  size(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))
*/
