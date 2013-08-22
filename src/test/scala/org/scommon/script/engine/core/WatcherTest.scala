package org.scommon.script.engine.core

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io.{PrintWriter, File}
import java.nio.file.{Path, Paths}

import scala.collection._
import scala.concurrent.duration._
import java.util.concurrent.Semaphore
import org.apache.commons.io.FileUtils
import java.util.concurrent.TimeUnit

/**
 * @author David Hoyt &lt;dhoyt@hoytsoft.org&gt;
 */
@RunWith(classOf[JUnitRunner])
class WatcherTest extends FunSuite with ShouldMatchers with BeforeAndAfterAll
{
  val TEMP_DIR = Paths.get(System.getProperty("java.io.tmpdir"), "directory-watcher-test").toFile

  override protected def beforeAll() {
    TEMP_DIR.exists() || TEMP_DIR.mkdirs() should be (true)
  }

  override protected def afterAll() {
    FileUtils.deleteDirectory(TEMP_DIR)
  }

  def createDir(root: Path = Paths.get(TEMP_DIR.toURI), path: String = ""): Path = {
    val p = root.resolve(path)
    val f = p.toFile
    if (f.exists())
      FileUtils.deleteDirectory(f)

    f.mkdirs() should be (true)
    p
  }

  def touch(f: File, time:Long = System.currentTimeMillis()): Boolean = {
    if (f.isDirectory) {
      if (!f.exists())
        f.mkdirs()
      f.setLastModified(time)
    } else {
      if (!f.exists()) {
        f.getParentFile.mkdirs()
        new PrintWriter(f).close()
      }
      f.setLastModified(time)
    }
  }

  test("Basic watcher for a directory works") {
    val dir = createDir(path = "basic-watcher-works")

    val wait_to_start = new Semaphore(0)
    val wait_to_stop = new Semaphore(0)
    val wait_for_event_received = new Semaphore(0)
    val future = Watcher(Seq(dir), 10.milliseconds) { (root, source, event) =>
      println(s"event: $event")
      event match {
        case Watcher.STARTED => wait_to_start.release()
        case Watcher.CREATED => wait_for_event_received.release()
        case Watcher.STOPPED => wait_to_stop.release()
        case _ =>
      }
    }

    //Wait 3 seconds and see if we've started.
    wait_to_start.tryAcquire(3L, TimeUnit.SECONDS) should be (true)

    val dirA = createDir(dir, "A")
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    touch(dirA.resolve("B").toFile)
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    createDir(dir, "C")
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    touch(dirA.resolve("D").toFile)
    wait_for_event_received.tryAcquire(10L, TimeUnit.SECONDS) should be (true)

    future.cancel(10.seconds) should be (true)

    wait_to_stop.tryAcquire(3L, TimeUnit.SECONDS) should be (true)
  }
}