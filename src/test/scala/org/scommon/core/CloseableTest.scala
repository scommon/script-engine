package org.scommon.core

import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, SeveredStackTraces, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import java.io._
import java.util.UUID

import java.util.concurrent.{TimeUnit, Semaphore}

import org.scommon.io._
import org.scommon.io.Utils._

@RunWith(classOf[JUnitRunner])
class CloseableTest extends FunSuite
                     with ShouldMatchers
                     with SeveredStackTraces
                     with BeforeAndAfterAll {

  val PARENT_WORKING_DIR = Path(s"closeable-test").toUserTemp

  override protected def beforeAll() {
    PARENT_WORKING_DIR.mkdirs()
  }

  override protected def afterAll() {
    PARENT_WORKING_DIR.deleteAll
  }

  test("Close is invoked correctly") {
    /**
     * MyClose has a method analagous to [[org.scommon.core.Closeable]]
     */
    class MyClose {
      private val lock = new Semaphore(0)

      def waitForClose(timeout:Long = 1000L, unit:TimeUnit = TimeUnit.MILLISECONDS) = {
        lock.tryAcquire(timeout, unit) should be (true)
      }

      def close():Unit = {
        lock.release()
      }

      def work() = {
        for(_ <- 0 until 10)
          Thread.sleep(1)
      }
    }

    //Basic idea is to do some work and ensure that MyClose#close() is called automatically by the
    //ResourceUtil.CloseableResource#foreach()

    val instance = new MyClose()
    for (mapped_instance <- instance)
      mapped_instance.work()

    //If things are wired up correctly, then the semaphore will be decremented allowing others to
    //continue execution.

    instance.waitForClose()

    val instance1 = new MyClose()
    val instance2 = new MyClose()
    for {
      mapped_instance1 <- instance1
      mapped_instance2 <- instance2
    } {
      mapped_instance1.work()
      mapped_instance2.work()
    }

    instance1.waitForClose()
    instance2.waitForClose()
  }

  test("Using and for comprehensions work correctly") {
    val working = Path(PARENT_WORKING_DIR, s"${UUID.randomUUID().toString}")
    val file1 = Path(working, "test1.txt")
    val file2 = Path(working, "test2.txt")

    try {
      file1.touch should be (true)
      file1.exists should be (true)

      file2.touch should be (true)
      file2.exists should be (true)

      using(new BufferedReader(new FileReader(file1))) { reader =>
        reader should not be (null)
      }

      using(new BufferedWriter(new FileWriter(file2))) { writer =>
        writer should not be (null)
        writer.write("Test output")
        writer.newLine()
      }

      //Read in a file and ensure that we use filters and maps.
      for {
        reader <- new BufferedReader(new FileReader(file2)) if reader.ready()
        line = reader.readLine()
      }
        line should be ("Test output")

      //All files should be closed at this point.
      //If they're not, then deleting the working directory should fail.
    } finally {
      working.deleteAll should be (true)
    }
  }

}
