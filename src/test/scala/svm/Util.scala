package svm

import java.io.DataInputStream

object Util {
  def loadClass(name: String) = {
    val stream = new DataInputStream(
      getClass.getResourceAsStream(s"/${name.replace(".", "/")}.class")
    )
    val bytes = new Array[Byte](stream.available())

    stream.readFully(bytes)
    model.Util.printClass(bytes)
    bytes
  }
  /*class SingleClassVirtualMachine(className: String, val classLoader: String => Array[Byte])
  extends VirtualMachine(classLoader){
    def run(main: String): Any = run(className.replace('.', '/'), main)
  }*/


}


