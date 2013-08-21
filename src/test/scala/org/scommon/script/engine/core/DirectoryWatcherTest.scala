package org.scommon.script.engine.core

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.nio.file.{Paths}

/**
 * @author David Hoyt &lt;dhoyt@hoytsoft.org&gt;
 */
@RunWith(classOf[JUnitRunner])
class DirectoryWatcherTest extends FunSuite with ShouldMatchers
{
  test("Basic directory watcher works") {
    val temp = Paths.get(System.getProperty("java.io.tmpdir"), "directory-watcher-test")
    val dirA = temp.resolve("A")
    dirA.toFile.mkdirs()

    val future = DirectoryWatcher(Seq(dirA))
    Thread.sleep(1000L * 60L * 5L)

    future.cancel()
  }
}