package metascala

import metascala.TestUtil._

/**
  * Created by lihaoyi on 17/8/17.
  */
object Main {
  val myVM = new VM(memorySize = 24 * 1014 * 1024).test{
    val x = new metascala.VM()
    x.invoke("metascala/features/ArrayTest", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
      .asInstanceOf[Array[Int]]
      .toSeq
  }

  def main(args: Array[String]): Unit = {
    System.in.read()
  }
}
