package ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @scala.annotation.tailrec def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def append[A](as: List[A], bs: List[A]): List[A] = foldRight(as, bs)(Cons(_, _))
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil:List[A])(append)

  def map[A,B](ls: List[A])(f: A => B): List[B] = foldRight(ls, Nil:List[B])((x, acc) => Cons(f(x), acc))

  def flatMap[A, B](as: List[A])(f: A => List[B]) : List[B] =
    foldRight(as, Nil:List[B])((x, acc) => append(f(x), acc))

  def printList[A](as: List[A]): Unit =
    as match {
      case Nil =>
      case Cons(x, Nil) => {
        print(x)
      }
      case Cons(x, xs) => {
        print(s"$x, ")
        printList(xs)
      }
    }
}
