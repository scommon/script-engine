package org.scommon.core

/**
 * Describes something with basic operations to satisfy a for comprehension and holds only
 * one piece of information (similar to [[scala.Option]]).
 *
 * @tparam T The type of data that is provided to the for comprehension.
 */
trait OptionFilterable[+T] { self =>
  def map[B](fn: T => B): B
  def flatMap[B](fn: T => Option[B]): Option[B]
  def filter(fn: T => Boolean): Option[T]
  def foreach(fn: T => Unit):Unit

  /** See [[http://scala-programming-language.1934581.n4.nabble.com/Rethinking-filter-td2009215.html#a2009218]]
    * for more information.
    */
  @inline final def withFilter(fn: T => Boolean): WithFilter = new WithFilter(fn)

  /** See [[http://scala-programming-language.1934581.n4.nabble.com/Rethinking-filter-td2009215.html#a2009218]]
    * for more information.
    */
  class WithFilter(p: T => Boolean) {
    def map[B](fn: T => B): Option[B] = self filter p map fn
    def flatMap[B](fn: T => Option[B]): Option[B] = self filter p flatMap fn
    def foreach[U](fn: T => U): Unit = self filter p foreach fn
    def withFilter(q: T => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }
}