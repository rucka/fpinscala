import scala.annotation.tailrec
import ch6.State._

object Exercise11 {
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  def update = (input: Input) => (state: Machine) => (input, state) match {
    case (Coin, Machine(true, cn, co)) if cn > 0 => Machine(false, cn, co + 1)
    case (Turn, Machine(false, cn, co)) if cn > 0 => Machine(true, cn - 1, co)
    case (_, s) => s
  }

  def refreshResult = get[Machine].map(s => (s.coins, s.candies))

  def startMachine(): State[Machine, (Int, Int)] =
    unit[Machine, (Int, Int)]((0,0))
      .flatMap(_ => refreshResult)

  def simulateStepMachine(i: Input): State[Machine, (Int, Int)] = {
    val state: State[Machine, (Int, Int)] = startMachine()
    simulateStepMachine(state, i)
  }

  def simulateStepMachine(state: State[Machine, (Int, Int)], i: Input): State[Machine, (Int, Int)] = {
    val up = update(i)
    state
      .flatMap(_ => modify(up))
      .flatMap(_ => get)
      .map(s => (s.coins, s.candies))
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    var m: (Machine => Machine) => State[Machine, Unit] = modify[Machine] _
    for {
      _ <- sequence(inputs.map(m compose update))
      s <- get
    } yield (s.coins, s.candies)
  }
}

import Exercise11._

/*
from repl you can test typing:
:load src/main/scala/fpinscala/ch6/State.scala
:load src/main/scala/fpinscala/ch6/Exercise11.scala
simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10))
simulateStepMachine(Coin).run(Machine(true, 10, 5))
*/
