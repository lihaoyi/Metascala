package svm

import model.{ClassFile, ConstantInfo}
import java.io.DataInputStream
import java.nio.ByteBuffer

class VirtualMachine{
  val threads = List[VmThread](VmThread())
  var heap = Set.empty[Object]
  var classes = Map.empty[String, Class]
                   .withDefault(name => loadClass(name))

  def loadClass(name: String) = {
    val stream = new DataInputStream(getClass.getResourceAsStream(name))
    val bytes = new Array[Byte](stream.available())
    stream.readFully(bytes)
    val classData = ClassFile.read(ByteBuffer.wrap(bytes))
    new Class(classData)
  }


}

case class VmThread(
  var pc: Int = 0,
  var stack: List[Frame] = List()
)

class Frame(
  var locals: Seq[Any],
  var frameStack: List[Any],
  var constantPool: Seq[ConstantInfo]
)

