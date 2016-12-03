import Par._
object ch7_12 {
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = es => {
    val k = run(es)(key).get
    choices(k)(es)
  }
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    val map = ((0 to choices.length - 1) zip choices)
      .foldRight(Map[Int, Par[A]]())((a, acc) => acc + a)
    choiceMap(n)(map)
  }
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(map(cond)(c => if (c) 0 else 1))(List(t, f))
}
import ch7_12._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch7/Par.scala
:load src/main/scala/fpinscala/ch7/Exercise12.scala
*/
