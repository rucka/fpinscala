package ch3
import scala.annotation.tailrec

object Exercise26 {
  def maximum(tree: Tree[Int]): Int = tree match {
    case Branch(l, r) => maximum(l) max maximum(r)
    case Leaf(v) => v
  }
}

import Exercise26._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/Tree.scala
  :load src/main/scala/fpinscala/ch3/Exercise26.scala
  maximum(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))
  maximum(Branch(Leaf(3),Branch(Leaf(8), Leaf(2))))
  maximum(Branch(Leaf(3),Branch(Leaf(1), Leaf(8))))
*/
