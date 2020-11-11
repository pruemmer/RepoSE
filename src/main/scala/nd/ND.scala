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
trait NDSearch {

  trait ValueEnum[T]

  /**
   * Start a non-deterministic computation.
   */
  def search[Result](comp : => Unit) : Option[Result]

  /**
   * Recursively start a non-deterministic computation, assume that it
   * is successful, and return the produced result. This method can be
   * used to control back-tracking, similarly as a cut in Prolog, since
   * choice points within the computation will be discarded once the first
   * result has been found.
   */
  def find[Result](comp : => Unit) : Result =
    assumeIsDefined(search[Result](comp))

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
   * Mark that computation at this point can take <code>n</code> alternative
   * paths. <code>comp</comp> will usually be written as a <code>match</code>
   * block.
   */
  def alternatives(n : Int)(comp : Int => Unit) : Unit =
    chooseInt(0 until n)(comp)

  /**
   * Recursively start a non-deterministic computation to find some
   * integer for which a computation succeeds. This method can be
   * used to control back-tracking, similarly as a cut in Prolog, since
   * choice points within the computation will be discarded once the first
   * result has been found.
   */
  def findInt[Result](r : Range)(comp : Int => Unit) : Result =
    find[Result] { chooseInt(r)(comp) }

  /**
   * Assume that the given condition is true, block program execution
   * otherwise.
   */
  def assume(f : Boolean) : Unit

  /**
   * Assume that the given option is not empty, and return its contents.
   */
  def assumeIsDefined[Data](v : Option[Data]) : Data = {
    assume(v.isDefined)
    v.get
  }

  /**
   * Program execution has succeeded.
   */
  def success[Result](r : Result) : Unit

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
 * Functions controlling alternating (existential/universal) execution.
 */
trait AlternatingSearch extends NDSearch {

  /**
   * Choose the minimum value in the given integer range for which program
   * execution will succeed.
   */
  def chooseMinInt(r : Range)(comp : Int => Unit) : Unit

  /**
   * Choose the maximum value in the given integer range for which program
   * execution will succeed.
   */
  def chooseMaxInt(r : Range)(comp : Int => Unit) : Unit

  /**
   * Recursively start a non-deterministic computation to find the minimum
   * integer for which some computation succeeds.
   */
  def findMinInt[Result](r : Range)(comp : Int => Unit) : Result =
    find[Result] { chooseMinInt(r)(comp) }

  /**
   * Recursively start a non-deterministic computation to find the maximum
   * integer for which some computation succeeds.
   */
  def findMaxInt[Result](r : Range)(comp : Int => Unit) : Result =
    find[Result] { chooseMaxInt(r)(comp) }

  /**
   * Assume that the given predicate holds for all elements of a collection.
   */
  def assumeForall[T](coll : Iterable[T])(pred : T => Boolean) : Unit

}

/**
 * Functions controlling non-deterministic execution with the help of
 * back-tracking.
 */
trait BacktrackingSearch extends AlternatingSearch {

  private object BacktrackingException                    extends Exception
  private case class SuccessException[Result](r : Result) extends Exception

  def search[Result](comp : => Unit) : Option[Result] =
    try {
      comp
      throw new Exception("search did not yield a result, \"success\" missing?")
    } catch {
      case SuccessException(result : Result) =>
        Some(result)
      case BacktrackingException =>
        None
    }

  def success[Result](r : Result) : Unit =
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

  def chooseMaxInt(r : Range)(comp : Int => Unit) : Unit =
    chooseInt(r.reverse)(comp)

  def assumeForall[T](coll : Iterable[T])(pred : T => Boolean) : Unit =
    assume(coll forall pred)

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
    throw new Exception("control most not leave \"choose\" statement")
  }

  def assume(f : Boolean) : Unit =
    if (!f)
      throw BacktrackingException

}
