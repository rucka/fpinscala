object ch12 {
  import scala.language.higherKinds
  import lib_monad.Functor

  trait Applicative[F[_]] extends Functor[F] {
    def unit[A](a: => A): F[A]
    def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
      apply(map(fa)(f.curried))(fb)
    def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
      map2(fab, fa)(_(_))

    def map[A,B](fa: F[A])(f: A => B): F[B] =
      apply(unit(f))(fa)
    def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, mbs) => map2(f(a), mbs)(_ :: _))
    def sequence[A](fas: List[F[A]]): F[List[A]] =
      traverse(fas)(identity)
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      sequence(List.fill(n)(fa))
    def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] =
      map2(fa, fb)((_,_))
    def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
      val self = this
      new Applicative[({type f[x] = (F[x], G[x])})#f] {
        def unit[A](a: => A) = (self.unit(a), G.unit(a))
        override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
          (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
      }
    }
    def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
      val self = this
      new Applicative[({type f[x] = F[G[x]]})#f] {
        def unit[A](a: => A) = self.unit(G.unit(a))
        override def map2[A,B,C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) =
          self.map2(fga, fgb)(G.map2(_,_)(f))
      }
    }
  }

  trait Traverse[F[_]] extends Functor[F] {
    def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
      sequence(map(fa)(f))
    def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
      traverse(fga)(ga => ga)
  }
  val applicativeOption = new Applicative[Option] {
    def unit[A](a: => A): Option[A] = Some(a)
  }
  val applicativeList = new Applicative[List] {
    def unit[A](a: => A): List[A] = List(a)
  }
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
