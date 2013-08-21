package org.scommon.script.engine.core

import java.io.File
import java.nio.file._
import java.nio.file.StandardWatchEventKinds._
import java.nio.file.attribute.BasicFileAttributes

import scala.util.control.Breaks._
import scala.language.implicitConversions
import java.util.concurrent.{Callable, ThreadFactory, Executors, ExecutorService}

object DirectoryWatcher {
  implicit def files2Watchable(files: Iterable[File]): Iterable[Watchable] =
    files.map(x => Paths.get(x.toURI))

  def apply(paths: Iterable[Watchable]): DirectoryWatcherFuture =
    new DirectoryWatcher(paths)
}

trait DirectoryWatcherFuture {
  def cancel():Unit
}

class DirectoryWatcher(
  paths: Iterable[Watchable],
  private[this] val threadFactory: ThreadFactory = Executors.defaultThreadFactory()
) extends DirectoryWatcherFuture {

  private val watcher = FileSystems.getDefault().newWatchService()
  private var keys: Map[WatchKey, Watchable] = Map.empty
  private val executor = Executors.newSingleThreadExecutor(threadFactory)

  for (path <- paths) path match {
    case x: Path => registerPath(x)
    case x: Watchable => register(x)
  }

  executor.submit(new Callable[Unit] {
    def call(): Unit = {
      try {
        run()
      } catch {
        case _:InterruptedException =>
          Thread.currentThread().interrupt()
      }
    }
  })

  def cancel():Unit =
    executor.shutdownNow()

  private[this] def register(path: Watchable) {
    keys += path.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY) -> path
  }

  private[this] def registerPath(path: Path) {
    Files.walkFileTree(path, new SimpleFileVisitor[Path]() {
      override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes) = {
        register(dir)
        FileVisitResult.CONTINUE
      }
    })
  }

  private[this] def run() =  breakable {
    while (true) {
      // wait for key to be signalled
      var key: WatchKey = null
      try {
        key = watcher.take();
      } catch {
        case _ => break
      }

      keys.get(key) match {
        case Some(dir) => {
          for (event: WatchEvent[_] <- key.pollEvents()) {
            val kind = event.kind()

            if (kind != OVERFLOW) {
              val ev = event.asInstanceOf[WatchEvent[_]]
              val name = ev.context()
              val child = dir.resolve(name)

              // broadcast event to all listeners
//              if (kind == ENTRY_CREATE) lestenerRouter ! Create(child)
//              else if (kind == ENTRY_DELETE) lestenerRouter ! Delete(child)
//              else if (kind == ENTRY_MODIFY) lestenerRouter ! Modify(child)

              // if directory is created, and watching recursively, then
              // register it and its sub-directories
              if (recursive && (kind == ENTRY_CREATE)) {
                try {
                  if (Files.isDirectory(child, LinkOption.NOFOLLOW_LINKS))
                    registerAll(child);
                } catch {
                  case _ =>
                }
              }
            }
          }

          // reset key and remove from set if directory no longer accessible
          if (!key.reset()) {
            keys = keys - key

            // all directories are inaccessible
            if (keys.isEmpty) break
          }
        }
        case None =>
      }
    }
  }
}