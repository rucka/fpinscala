object ch12_13 {
  import scala.language.higherKinds
  import ch12.{Traverse, Applicative, applicativeList}

  case class Tree[+A](head: A, tail: List[Tree[A]])
  val optionTraverse = new Traverse[Option] {
    def map[A,B](fa: Option[A])(f: A=>B): Option[B] = fa map f
    override def traverse[G[_],A,B](oa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      oa match {
        case Some(a) => G.map(f(a))(Some(_))
        case _ => G.unit(None)
      }
  }
  val listTraverse = new Traverse[List] {
    def map[A,B](ta: List[A])(f: A=>B): List[B] = ta.foldRight(List[B]())(f(_) :: _)
    override def traverse[G[_],A,B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, acc) => G.map2(f(a), acc)(_ :: _))
  }
  val treeTraverse = new Traverse[Tree] {
    def map[A,B](ta: Tree[A])(f: A=>B): Tree[B] =
      Tree(
        f(ta.head),
        ta.tail.foldRight(List[Tree[B]]())(map(_)(f) :: _)
      )

    override def traverse[G[_],A,B](ta: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(ta.head), listTraverse.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }
}
import ch12_13._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/lib/Monad.scala
:load src/main/scala/fpinscala/ch12/Applicative.scala
:load src/main/scala/fpinscala/ch12/Exercise13.scala
*/
