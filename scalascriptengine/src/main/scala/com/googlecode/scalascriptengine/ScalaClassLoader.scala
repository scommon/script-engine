package com.googlecode.scalascriptengine

import java.io.File
import java.net.URL
import java.net.URLClassLoader

/**
 * a throwaway classloader that keeps one version of the source code. For every code change/refresh,
 * a new instance of this classloader is used.
 *
 * @author kostantinos.kougios
 *
 * 22 Dec 2011
 */
class ScalaClassLoader(sourceDirs: Set[File], classPath: Set[File], parentClassLoader: ClassLoader)
	extends URLClassLoader((classPath ++ sourceDirs).toArray.map(_.toURI.toURL), parentClassLoader) {

	def this(sourceDirs: Set[File], classPath: Set[File]) = this(sourceDirs, classPath, Thread.currentThread.getContextClassLoader)
	def this(sourceDir: File, classPath: Set[File]) = this(Set(sourceDir), classPath)

	def get[T](className: String): Class[T] = loadClass(className).asInstanceOf[Class[T]]
	def newInstance[T](className: String): T = get(className).newInstance.asInstanceOf[T]
}
