package org.scommon.core

trait CustomizableComprehensionForEach {
  def foreach[T, U](value:T, fn: T => U):U
}

/**
 * Describes something with basic operations to satisfy a for comprehension and holds only
 * one piece of information (similar to [[scala.Option]]).
 *
 * @tparam T The type of data that is provided to the for comprehension.
 */
trait ZeroOrOneCustomizableComprehension[+T] { self =>
  import ZeroOrOneCustomizableComprehension._

  def get:T
  def isEmpty:Boolean
  protected def customForEach:Option[CustomizableComprehensionForEach]

  def map[B](fn: T => B): ZeroOrOneCustomizableComprehension[B] =
    if (isEmpty) NotCustomizedComprehension else CustomizedComprehension[B](fn(this.get), customForEach)

  def flatMap[B](fn: T => ZeroOrOneCustomizableComprehension[B]): ZeroOrOneCustomizableComprehension[B] =
    if (isEmpty) NotCustomizedComprehension else fn(this.get)

  def filter(fn: T => Boolean): ZeroOrOneCustomizableComprehension[T] =
    if (isEmpty || fn(this.get)) this else NotCustomizedComprehension

  def foreach[U](fn: T => U):Unit =
    if (!isEmpty) customForEach.map(_.foreach[T, U](this.get, fn))

  /** See [[http://scala-programming-language.1934581.n4.nabble.com/Rethinking-filter-td2009215.html#a2009218]]
    * for more information.
    */
  @inline final def withFilter(fn: T => Boolean): WithFilter = new WithFilter(fn)

  /** See [[http://scala-programming-language.1934581.n4.nabble.com/Rethinking-filter-td2009215.html#a2009218]]
    * for more information.
    */
  class WithFilter(p: T => Boolean) {
    def map[B](fn: T => B): ZeroOrOneCustomizableComprehension[B] = self filter p map fn
    def flatMap[B](fn: T => ZeroOrOneCustomizableComprehension[B]): ZeroOrOneCustomizableComprehension[B] = self filter p flatMap fn
    def foreach[U](fn: T => U): Unit = self filter p foreach fn
    def withFilter(q: T => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }
}

object ZeroOrOneCustomizableComprehension {
  final case class CustomizedComprehension[+T](transientValue: T, protected val customForEach: Option[CustomizableComprehensionForEach]) extends ZeroOrOneCustomizableComprehension[T] {
    val isEmpty = false
    val get = transientValue
  }

  case object NotCustomizedComprehension extends ZeroOrOneCustomizableComprehension[Nothing] {
    protected val customForEach = None
    val isEmpty = true
    def get = throw new NoSuchElementException("NotCustomizedComprehension.get")
  }
}
