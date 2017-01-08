object ch11_5 {
  import ch11.{listMonad, optionMonad}
  val replicaList = listMonad.replicateM(2, List(1,2,3))
  val replicaNone = optionMonad.replicateM(2, None)
  val replicaSome = optionMonad.replicateM(2, Some(4))
}
import ch11_5._
/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch11/Monad.scala
:load src/main/scala/fpinscala/ch11/Exercise5.scala
*/
