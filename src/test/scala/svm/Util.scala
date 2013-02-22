package svm

import java.io.{IOException, DataInputStream}

object Util {
  def loadClass(name: String) = {
    val slashName = s"/${name.replace(".", "/")}.class"
    println("loading class " + slashName)
    val loaded = getClass.getResourceAsStream(slashName)
    if (loaded == null) throw new IOException("Can't find file " + slashName)
    val stream = new DataInputStream(loaded)
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


