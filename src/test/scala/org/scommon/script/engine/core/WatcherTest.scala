package org.scommon.script.engine.core

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.io.{PrintWriter, File}
import java.nio.file.{WatchEvent, Path, Paths}
import java.nio.file.StandardWatchEventKinds._

import scala.collection._
import scala.concurrent.duration._
import java.util.concurrent.{TimeUnit, CountDownLatch}
import org.apache.commons.io.FileUtils

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

  @inline def touch(f: File, time:Long = System.currentTimeMillis()): Boolean = {
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

    val latch = new CountDownLatch(2)
    val future = Watcher(Seq(dir), 10.milliseconds) { (root, source, kind) =>
      println(s"KIND: $kind")
      if (kind == ENTRY_CREATE)
        latch.countDown()
    }

    createDir(dir, "A")
    touch(dir.resolve("B").toFile)

    latch.await(30L, TimeUnit.SECONDS)

    future.cancel()
  }
}