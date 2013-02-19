package svm

import java.io.DataInputStream

/**
 * Created with IntelliJ IDEA.
 * User: Haoyi
 * Date: 2/19/13
 * Time: 12:49 AM
 * To change this template use File | Settings | File Templates.
 */
object Util {
  def loadClass(name: String) = {
    println("Looking for Class: " + name)
    val stream = new DataInputStream(
      getClass.getResourceAsStream(s"/${name.replace('.', '/')}.class")
    )
    val bytes = new Array[Byte](stream.available())
    stream.readFully(bytes)
    bytes
  }
  class SingleClassVirtualMachine(className: String, val classLoader: String => Array[Byte])
  extends VirtualMachine(classLoader){
    def run(main: String): Any = run(className, main)
  }


}
