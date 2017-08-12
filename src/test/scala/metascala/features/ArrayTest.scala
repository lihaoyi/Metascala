package metascala
package features

import org.scalatest.FreeSpec


object ArrayTest{
  def bubbleSort(arr: Array[Int]) = {
    for(i <- 1 until arr.length) {
      for(j <- 1 until arr.length){
        if (arr(j-1) > arr(j)){
          val temp = arr(j)
          arr(j) = arr(j-1)
          arr(j-1) = temp
        }
      }
    }
    arr
  }
}
class ArrayTest extends FreeSpec{
  import Util._
  val tester = new VM()
  "array stuff" - {
    "makeIntArray" in {
      for(i <- Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)){
        tester.test(new Array[Int](i))
      }
    }
    "makeFloatArray" in tester.test{
      val arr = new Array[Float](3)
      arr(0) = 0.25f; arr(1) = 0.5f; arr(2) = 0.75f
      arr
    }
    "makeStringArray" in tester.test{
      val arr = new Array[String](3)
      arr(0) = "omg"; arr(1) = "wtf"; arr(2) = "bbq"
      arr
    }
    "longArrayOps" in {
      for(i <- Seq(0, 1, 2)){
        tester.test{
          val arr = new Array[Long](3)
          arr(0) = 12345; arr(1) = 67890; arr(2) = 12345
          arr(i)
        }
      }
    }
    "doubleArrayOps" in {
    val in = Array(2.1, 2.72)
      tester.test{
        for(i <- 0 until in.length){
          in(i) = in(i) + i
        }
        for(i <- 0 until in.length){
          in(i) = in(i) + i
        }
        in
      }
    }

    "arrayLength" in tester.test{
      val arr0 = new Array[String](0)
      val arr1 = new Array[Int](5)
      arr1(0) = 1; arr1(1) = 2; arr1(2) = 3; arr1(3) = 4; arr1(4) = 5
      val arr2 = new Array[Double](5)
      arr2(0) = 0.1; arr2(1) = 0.2; arr2(2) = 0.3; arr2(3) = 0.4
      arr0.length + arr1.length + arr2.length
    }
    "arraySet" in tester.test{
      val arr = new Array[Int](10)
      for(i <- 0 until 10){
        arr(i) = i
      }
      arr
    }
    "arrayGet" in tester.test{
      var total = 0
      val arr = new Array[Int](10)
      for(i <- 0 until 10){
        arr(i) = i
      }
      for(i <- 0 until 10){
        total = total + 1000 * arr(i)
      }
      total
    }
    "getSet" in tester.test{
      val buf = new Array[Char](10)
      val digits = new Array[Char](10)
      digits(0) = '0';  digits(1) = '1'
      digits(2) = '2';  digits(3) = '3'
      digits(4) = '4';  digits(5) = '5'
      digits(6) = '6';  digits(7) = '7'
      digits(8) = '8';  digits(9) = '9'
      var charPos = buf.length
      var r = 10
      while(charPos > 0){
        r -= 1
        charPos -= 1
        buf(charPos) = digits(r)
      }
      buf(r + 5)
    }
    "bubbleSort" in {
//      val tester = new Tester("metascala.features.ArrayTest")

      val inputs = Seq(
        Array(0, 1, 2, 3, 4, 5, 6, 7),
        Array(7, 6, 5, 4, 3, 2, 1, 0),
        Array(0, 1, 2, 3, 4, 5, 6, 7),
        Array.fill(10)(util.Random.nextInt()),
        Array.fill(20)(util.Random.nextInt())
      )
      for (input <- inputs) tester.test{
        ArrayTest.bubbleSort(input)
      }
    }

  }
  "multi dim arrays" - {
    val buffer = new BufferLog(4000)
    "make2D" in {
      for(i <- Seq(0, 1, 2)) tester.test{
        metascala.features.arrays.MultiDimArrays.make2D(i, i)
      }
    }

    "make3D" in {
      for(i <- Seq(0, 1, 2)) tester.test{
        metascala.features.arrays.MultiDimArrays.make3D(
          i, i, i
        )
      }
    }
    "getAndSet" in tester.test{
      metascala.features.arrays.MultiDimArrays.getAndSet
    }
  }
}