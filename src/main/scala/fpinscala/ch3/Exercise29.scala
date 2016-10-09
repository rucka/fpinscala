package ch3
import scala.annotation.tailrec

object Exercise29 {
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)

  def depth(tree: Tree[Int]): Int = fold(tree)(_ => 0)((x, y) => 1 + (x max y))

  def maximum(tree: Tree[Int]): Int = fold(tree)(x => x)(_ max _)

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}

import Exercise29._

/*
from repl you can test typing:
  :load src/main/scala/fpinscala/ch3/Tree.scala
  :load src/main/scala/fpinscala/ch3/Exercise29.scala
  size(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))
  maximum(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))
  maximum(Branch(Leaf(3),Branch(Leaf(8), Leaf(2))))
  maximum(Branch(Leaf(3),Branch(Leaf(1), Leaf(8))))
  depth(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))
  depth(Branch(Leaf(3),Leaf(2)))
  map(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))(_ + 1)
  map(Branch(Leaf(3),Branch(Leaf(1), Leaf(2))))(_ * 2)
*/
