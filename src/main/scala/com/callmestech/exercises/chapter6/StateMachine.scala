package com.callmestech.exercises.chapter6

object StateMachine {

  /** Exercise 6.10
   *
   * Hard: To gain experience with the use of State,
   * implement a finite state automaton that models a simple candy dispenser.
   * The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
   * It can be in one of two states: locked or unlocked.
   * It also tracks how many candies are left and how many coins it contains.
   *
   * The rules of the machine are as follows:
   * <ul>
   * <li>Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.</li>
   * <li> Turning the knob on an unlocked machine will cause it to dispense candy and become locked.</li>
   * <li>Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.</li>
   * <li> A machine that’s out of candy ignores all inputs.</li>
   * </ul>
   * */

  sealed trait Input

  object Input {
    case object Coin extends Input
    case object Turn extends Input
  }

  object Candy {
    import Input._

    def update: Input => Machine => Machine = (i: Input) => (s: Machine) =>
      (i, s) match {
        case (_, Machine(_, 0, _))              => s
        case (Coin, Machine(true, _, coins))    => s.copy(locked = false, coins = coins + 1)
        case (Coin, Machine(false, _, _))       => s
        case (Turn, Machine(true, _, _))        => s
        case (Turn, Machine(false, candies, _)) => s.copy(locked = true, candies = candies - 1)
      }

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      for {
        _ <- State.sequence(inputs.map(State.modify[Machine] _ compose(update)))
      s   <- State.get
      } yield (s.coins, s.candies)
    }
  }


  case class Machine(locked: Boolean, candies: Int, coins: Int)
}
