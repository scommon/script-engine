package org.scommon

import java.io.{OutputStream, File, InputStream}
import org.scommon.core.CloseableIterator

package object io {

  @inline implicit class PathUtilFileExtensions(f: File) {
    /** @see [[org.scommon.io.PathUtil#tempDirectory]] */
    @inline def tempDirectory: String = PathUtil.tempDirectory

    /** @see [[org.scommon.io.PathUtil.userTempDirectory]] */
    @inline def userTempDirectory: String = PathUtil.userTempDirectory

    /** @see [[org.scommon.io.PathUtil.toTemp()]] */
    @inline def toTemp: File = PathUtil.toTemp(f)

    /** @see [[org.scommon.io.PathUtil.toUserTemp()]] */
    @inline def toUserTemp: File = PathUtil.toUserTemp(f)

    /** @see [[org.scommon.io.PathUtil.deleteAll()]] */
    @inline def deleteAll: Boolean = PathUtil.deleteAll(f)
  }

  @inline implicit class FileUtilFileExtensions(f: File) {
    /** @see [[org.scommon.io.FileUtil.touch()]] */
    @inline def touch: Boolean = FileUtil.touch(f)
    @inline def touch(time: Long): Boolean = FileUtil.touch(f, time)
    @inline def open: OutputStream = openForWrite()
    @inline def openForRead(): InputStream = FileUtil.openForRead(f)
    @inline def openForWrite(): OutputStream = FileUtil.openForWrite(f)
  }

  @inline implicit class InputStreamExtensions(input: InputStream) {
    @inline def toIterator: CloseableIterator[(Int, Array[Byte])] =
      InputStreamUtil.toIterator(input)
    @inline def toIterator(bufferSize: Int = 1024): CloseableIterator[(Int, Array[Byte])] =
      InputStreamUtil.toIterator(input, bufferSize)
  }
}
