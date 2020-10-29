/**
 * This file is part of Scala-ND.
 *
 * Copyright (C) 2020 Philipp Ruemmer <ph_r@gmx.net>
 *
 * Scala-ND is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Scala-ND is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Scala-ND.  If not, see <http://www.gnu.org/licenses/>.
 */

package nd

/**
 * Functions controlling non-deterministic execution.
 */
trait NDSearch[Result] {

  trait ValueEnum[T]

  /**
   * Start a non-deterministic computation.
   */
  def search(comp : => Unit) : Option[Result]

  /**
   * Non-deterministically choose a value of type <code>T</code> and continue
   * program execution.
   */
  def choose[T](comp : T => Unit)(implicit enum : ValueEnum[T]) : Unit

  /**
   * Non-deterministically choose a value in the given integer range and
   * continue program execution.
   */
  def chooseInt(r : Range)(comp : Int => Unit) : Unit

  /**
   * Choose the minimum value in the given integer range for which program
   * execution will succeed.
   */
  def chooseMinInt(r : Range)(comp : Int => Unit) : Unit

  /**
   * Assume that the given condition is true, block program execution
   * otherwise.
   */
  def assume(f : Boolean) : Unit

  /**
   * Program execution has succeeded.
   */
  def success(r : Result) : Unit

  /**
   * Program execution has reached a dead end.
   */
  def sorry : Unit = assume(false)

  // Some alternative function names.

  def wishFor(f : Boolean) : Unit = assume(f)
  def abort : Unit   = sorry
  def failure : Unit = sorry

}

/**
 * Functions controlling non-deterministic execution with the help of
 * back-tracking.
 */
trait BacktrackingSearch[Result] extends NDSearch[Result] {

  private object BacktrackingException            extends Exception
  private case class SuccessException(r : Result) extends Exception

  def search(comp : => Unit) : Option[Result] =
    try {
      comp
      None
    } catch {
      case SuccessException(result) => Some(result)
      case BacktrackingException    => None
    }

  def success(r : Result) : Unit =
    throw new SuccessException (r)

  trait BTValueEnum[T] extends ValueEnum[T] {
    def iterator : Iterator[T]
  }

  implicit object BooleanEnum extends BTValueEnum[Boolean] {
    def iterator = Iterator(false, true)
  }

  def choose[T](comp : T => Unit)(implicit enum : ValueEnum[T]) : Unit =
    chooseFromIterator(enum.asInstanceOf[BTValueEnum[T]].iterator, comp)

  def chooseInt(r : Range)(comp : Int => Unit) : Unit =
    chooseFromIterator(r.iterator, comp)

  def chooseMinInt(r : Range)(comp : Int => Unit) : Unit =
    chooseInt(r)(comp)

  private def chooseFromIterator[T](it : Iterator[T],
                                    comp : T => Unit) : Unit = {
    var next = it.next

    while (it.hasNext)
      try {
        val n = next
        next = it.next
        comp(n)
      } catch {
        case BacktrackingException => // use next value
      }

    // last possible value
    comp(next)
  }

  def assume(f : Boolean) : Unit =
    if (!f)
      throw BacktrackingException

}
