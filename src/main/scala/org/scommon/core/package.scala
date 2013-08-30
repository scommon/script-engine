package org.scommon

import java.io.{OutputStream, File, InputStream}

import org.joda.time.{ReadableDateTime, ReadableInstant}
import org.joda.time.{DateTime => JodaDateTime}

import scala.language.reflectiveCalls
import scala.language.implicitConversions

/**
 */
package object core {
  val readableInstantOrdering = implicitly[Ordering[ReadableInstant]]

  implicit val ReadableDateTimeOrdering = new Ordering[ReadableDateTime] {
    def compare(a:ReadableDateTime, b:ReadableDateTime) = a.compareTo(b)
  }

  implicit val DateTimeOrdering = new Ordering[JodaDateTime] {
    def compare(a:JodaDateTime, b:JodaDateTime) = a.compareTo(b)
  }

  /**
   * Provides functionality similar to C#'s default keyword.
   * However, default is now pimped -- you can do much more with it
   * than you can C#'s.
   */
  def default[A: Default] = implicitly[Default[A]].value

  /**
   * Provides functionality similar to C#'s default keyword.
   * Use when default[A] doesn't work.
   *
   * Alternative is to use "null.asInstanceOf[A]" which will
   * accomplish the same task.
   */
  def defaultValue[A] = {
    class Temp {
      var default_value: A = _
    }
    (new Temp).default_value
  }

    /**
     * Structural type that says "anything with a close method that returns Unit".
     */
  type CloseableType = { def close():Unit }

  def using[A <: CloseableType, B](closeable: A)(body: A => B): B =
    try {
      body(closeable)
    } finally {
      closeable.close()
    }

  //The following allows us to use anything with a .close() method in a for comprehension w/
  //automatic cleanup.

  private[this] class CloseableResource[+T](resource: T, fnOnClose: T => Unit) extends OptionFilterable[T] {
    self =>

    @inline final def map[B](f: T => B): B =
      f(resource)

    @inline final def flatMap[B](fn: T => Option[B]): Option[B] =
      if (resource equals null) None else fn(resource)

    @inline final def filter(fn: T => Boolean): Option[T] =
      if (fn(resource)) Some(resource) else None

    @inline final def foreach(fn: T => Unit) = {
      val r = resource
      try {
        fn(r)
      } finally {
        fnOnClose(r)
      }
    }
  }

  /**
   * Implicitly converts something that's Closeable into a
   * [[org.scommon.script.engine.core.OptionFilterable]]. This should be picked up for use in for comprehensions and has no
   * real utility elsewhere.
   *
   * @param closeable An instance of a class that defines a .close() method as described by Closeable.
   * @tparam T Actual type of the Closeable.
   * @return A new instance of [[org.scommon.script.engine.core.OptionFilterable]].
   */
  @inline implicit def closeable2OptionFilterable[T <: CloseableType](closeable: T): OptionFilterable[T] =
    new CloseableResource[T](closeable, {_.close()})

  @inline implicit class OptionStringExtensions(s: Option[String]) {
    /** @see [[org.scommon.script.engine.core.StringUtil#isNoneOrEmpty(Option[String]))]] */
    @inline def isNoneOrEmpty: Boolean = StringUtil.isNoneOrEmpty(s)

    /** @see [[[org.scommon.script.engine.core.StringUtil#isNonEmpty(Option[String]))]]] */
    @inline def isNonEmpty: Boolean = StringUtil.isNonEmpty(s)

    /** @see [[org.scommon.script.engine.core.StringUtil#checked(Option[String]))]] */
    @inline def checked: String = StringUtil.checked(s)
  }

  @inline implicit class StringExtensions(s: String) {
    /** @see [[org.scommon.script.engine.core.StringUtil#isNullOrEmpty(String)]] */
    @inline def isNullOrEmpty: Boolean = StringUtil.isNullOrEmpty(s)

    /** @see [[org.scommon.script.engine.core.StringUtil#isNonEmpty(String)]] */
    @inline def isNonEmpty: Boolean = StringUtil.isNonEmpty(s)

    /** @see [[org.scommon.script.engine.core.StringUtil#checked(String)]] */
    @inline def checked: String = StringUtil.checked(s)

    /** @see [[org.scommon.script.engine.core.StringUtil#toValidIdentifier(String)]] */
    @inline def toValidIdentifier: String = StringUtil.toValidIdentifier(s)
  }
}