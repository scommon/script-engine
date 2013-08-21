package org.scommon.script.engine.core

import java.io.File
import java.nio.file._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes

import scala.util.control.Breaks._
import scala.concurrent.duration._
import scala.collection.JavaConversions._
import scala.language.implicitConversions
import java.util.concurrent.{Callable, ThreadFactory, Executors}
import scala.concurrent.duration.Duration

object Watcher {
  type EventReceived[TRoot <: Watchable, TSource <: Watchable] = (TRoot, TSource, WatchEvent.Kind[_]) => Unit

  trait EventMagnet[TRoot <: Watchable, TSource <: Watchable] {
    def eventReceived(root: TRoot, source: TSource, kind: WatchEvent.Kind[_]): Unit = {}
  }

  trait EventListener[TRoot <: Watchable, TSource <: Watchable] extends EventMagnet[TRoot, TSource] {
    def created (root: TRoot, source: TSource):  Unit = {}
    def modified(root: TRoot, source: TSource): Unit = {}
    def deleted (root: TRoot, source: TSource):  Unit = {}
    override def eventReceived(root: TRoot, source: TSource, kind: WatchEvent.Kind[_]) = kind match {
      case ENTRY_MODIFY => modified(root, source)
      case ENTRY_CREATE => created(root, source)
      case ENTRY_DELETE => deleted(root, source)
    }
  }

  implicit val NOOP_EVENTRECEIVED: EventReceived[Watchable, Watchable] = (_, _, _) => {}
  implicit val NOOP_EVENTMAGNET: EventMagnet[Watchable, Watchable] = new EventMagnet[Watchable, Watchable] {}

  def singleEventReceived2EventMagnet[TRoot <: Watchable, TSource <: Watchable](
    callback: EventReceived[TRoot, TSource]
  ): EventMagnet[TRoot, TSource] = {
    new EventMagnet[TRoot, TSource] {
      override def eventReceived(root: TRoot, source: TSource, kind: WatchEvent.Kind[_]): Unit =
        callback(root, source, kind)
    }
  }

  implicit def multipleEventReceiveds2EventMagnet[TRoot <: Watchable, TSource <: Watchable](
    created:  EventReceived[TRoot, TSource] = NOOP_EVENTRECEIVED,
    modified: EventReceived[TRoot, TSource] = NOOP_EVENTRECEIVED,
    deleted:  EventReceived[TRoot, TSource] = NOOP_EVENTRECEIVED
  ): EventMagnet[TRoot, TSource] = {
    new EventListener[TRoot, TSource] {
      override def created (root: TRoot, source: TSource) = created (root, source)
      override def modified(root: TRoot, source: TSource) = modified(root, source)
      override def deleted (root: TRoot, source: TSource) = deleted (root, source)
    }
  }

  implicit def files2Watchable(files: TraversableOnce[File]): TraversableOnce[Watchable] =
    files.map(x => Paths.get(x.toURI))

  def apply[TWatchable <: Watchable](watched: TraversableOnce[TWatchable], fallbackDuration: Duration = 1.second)(implicit callback: EventReceived[TWatchable, TWatchable]): WatcherFuture[TWatchable] =
    apply[TWatchable](watched, fallbackDuration, Executors.defaultThreadFactory())(singleEventReceived2EventMagnet(callback))

  def apply[TWatchable <: Watchable](watched: TraversableOnce[TWatchable], fallbackDuration: Duration, threadFactory: ThreadFactory)(implicit listener: EventMagnet[TWatchable, TWatchable]): WatcherFuture[TWatchable] =
    new Watcher[TWatchable, TWatchable](watched, listener, fallbackDuration, threadFactory)
}

trait WatcherFuture[TWatchable <: Watchable] {
  def cancel():Unit
}

class Watcher[TWatchable <: Watchable, TSource <: Watchable] private (
  watched:          TraversableOnce[TWatchable],
  listener:         Watcher.EventMagnet[TWatchable, TSource],
  fallbackDuration: Duration,
  threadFactory:    ThreadFactory
) extends WatcherFuture[TWatchable] {

  require(fallbackDuration > 0.seconds, "The fallback duration must be greater than 0 seconds")

  private[this] val executor = Executors.newCachedThreadPool(threadFactory)
  private[this] val fallback_millis = fallbackDuration.toMillis
  private[this] val fallback_nanos: Int = (fallbackDuration.toNanos - fallback_millis.milliseconds.toNanos).toInt

  //Exception that we'll use to break out of the runner. Pre-create an instance that
  //we can re-use multiple times if necessary.
  private class WatchedCompletelyRemoved extends Throwable
  private object WatchedCompletelyRemoved {
    val instance = new WatchedCompletelyRemoved
  }

  //Because watched is a TraversableOnce[], be sure we iterate over the collection
  //only ONCE -- it's possible the collection can be traversed multiple times, but
  //we shouldn't make any assumptions.

  for (watch <- watched) {
    executor.submit(new Callable[Unit] {
      def call(): Unit = {
        try {
          var stop = false
          var existed = true
          while(!stop) {
            try {
              watch match {
                case x: Path =>
                  if (x.toFile.exists()) {
                    if (!existed) {
                      existed = true
                      listener.eventReceived(watch, watch.asInstanceOf[TSource], ENTRY_CREATE)
                    }
                    run(watch)
                  } else {
                    Thread.sleep(fallback_millis, fallback_nanos)
                  }
                case x: Watchable =>
                  run(watch)
              }
            } catch {
              case _:WatchedCompletelyRemoved =>
                existed = false
              case _:InterruptedException =>
                stop = true
            }
          }
        } catch {
          case _:InterruptedException =>
            //Do nothing, just exit.
          case t:Throwable =>
            throw t
        }
      }
    })
  }

  def cancel():Unit =
    executor.shutdownNow()

  private[this] def run(watch: TWatchable) = {
    var keys: Map[WatchKey, Watchable] = Map()
    var watcher:WatchService = null

    try {
      watcher = FileSystems.getDefault.newWatchService()

      def register(watchItem: Watchable) = {
        keys += watchItem.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY) -> watchItem
      }

      def registerPath(path: Path) = {
        Files.walkFileTree(path, new SimpleFileVisitor[Path]() {
          override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
            register(dir)
            FileVisitResult.CONTINUE
          }
        })
      }

      watch match {
        case x: Path => registerPath(x)
        case x: Watchable => register(x)
      }

      breakable {
        while (true) {
          //Wait for key to be signalled.
          var key: WatchKey = null
          try {
            key = watcher.take()
          } catch {
            case _:Throwable =>
              break()
          }

          keys.get(key) match {
            case Some(value) => {
              for (ev <- key.pollEvents().toIterable) {
                val kind = ev.kind()
                kind match {
                  case ENTRY_MODIFY | ENTRY_CREATE | ENTRY_DELETE =>
                    value match {
                      case dir:Path => {
                        val name = ev.context().asInstanceOf[Path]
                        val child = dir.resolve(name)

                        // if directory is created, and watching recursively, then
                        // register it and its sub-directories
                        if (kind == ENTRY_CREATE) {
                          try {
                            if (Files.isDirectory(child, LinkOption.NOFOLLOW_LINKS))
                              registerPath(child)
                          } catch {
                            case _:Throwable =>
                              break()
                          }
                        }
                      }
                    }

                    //let everyone know about it.
                    listener.eventReceived(watch, value.asInstanceOf[TSource], kind)
                }
              }

              //Reset key and remove from set if watched is no longer accessible
              if (!key.reset()) {
                keys = keys - key

                //All watches are inaccessible
                if (keys.isEmpty) {
                  throw WatchedCompletelyRemoved.instance
                }
              }
            }
            case None =>
          }
        }
      }
    } finally {
      if (watcher != null) {
        watcher.close()
      }
    }
  }
}