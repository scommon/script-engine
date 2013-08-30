package org.scommon.io

import java.io._

import scala.language.implicitConversions

object FileUtil {
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

  @inline def openForRead(f: File):InputStream =
    new FileInputStream(f)

  @inline def openForWrite(f: File, append: Boolean = false):OutputStream =
    new FileOutputStream(f, append)

  object File {
    def apply(parent:File, path:String):File =
      new File(parent, path)
  }
}
