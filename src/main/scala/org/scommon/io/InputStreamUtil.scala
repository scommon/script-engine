package org.scommon.io

import java.io.InputStream
import scala.Array
import org.scommon.core.CloseableIterator

object InputStreamUtil {
  def toIterator(input: InputStream, bufferSize: Int = 1024): CloseableIterator[(Int, Array[Byte])] = new CloseableIterator[(Int, Array[Byte])] {
    @volatile private[this] var read = 0

    override def hasDefiniteSize: Boolean = false
    override def isTraversableAgain: Boolean = false

    def close(): Unit = input.close()
    def hasNext: Boolean = read >= 0

    def next(): (Int, Array[Byte]) = {
      val buffer = new Array[Byte](1024)
      val bytes_read = input.read(buffer)
      read = bytes_read

      (bytes_read, buffer)
    }
  }
}
