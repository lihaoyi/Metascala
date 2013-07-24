package metascala

import metascala.imm.Type.Prim

class Heap(memorySize: Int)(implicit vm: VM){

  val memory = new Array[Int](memorySize * 2)
  var start = 0
  var freePointer = 1

  def allocate(n: Int) = {
    //      println(s"Allocating $n at $freePointer")
    if (freePointer + n > memorySize) collect(start)
    val newFree = freePointer
    freePointer += n
    newFree
  }

  def apply(n: Int): Int = memory(n)
  def update(n: Int, v: Int) = memory.update(n, v)

  def dump(start: Int = start, freePointer: Int = freePointer) = {
    s"Heap $start to $freePointer\n" +
      memory.slice(start, freePointer)
        .grouped(10)
        .map(_.map(_.toString.padTo(4, ' ')))
        .map(_.mkString)
        .mkString("\n")
  }


  def blit(freePointer: Int, src: Int) = {
    if (memory(src + 1) >= 0){
      if (src.isObj) {
        val obj = src.obj

        //          println(s"blit obj src: $src, free:$freePointer, length: $length")

        System.arraycopy(memory, src, memory, freePointer, obj.heapSize)
        memory(src + 1) = -freePointer
        (freePointer, freePointer + obj.heapSize)
      } else { // it's an Arr

        //          println(s"blit arr src: $src, free: $freePointer, length: $length")
        val length = src.arr.heapSize
        System.arraycopy(memory, src, memory, freePointer, length)
        memory(src + 1) = -freePointer
        (freePointer, freePointer + length)
      }
    }else{
      println(s"non-blitting $src -> ${-memory(src + 1)}")
      (-memory(src + 1), freePointer)
    }
  }
  def collect(from: Int){
    val to = (from + memorySize) % (2 * memorySize)
    for(i <- to until (to+memorySize)){
      memory(i) = 0
    }
    //      println("===============Collecting==================")

    def collectDump = {
      //        println("From")
      //        println(dump(from, from + memorySize))
      //        println("To")
      //        println(dump(to, to + memorySize))
    }
    val roots = vm.getRoots

    //      println(s"allRoots ${roots.map(_())}")

    var scanPointer = 0
    if(from == 0){
      freePointer = memorySize + 1
      scanPointer = memorySize + 1
    }else{ // to == memorySize
      start = 0
      freePointer = 1
      scanPointer = 1
    }


    for(root <- roots){
      val oldRoot = root()

      val (newRoot, nfp) = blit(freePointer, oldRoot)
      freePointer = nfp
      root() = newRoot
      //        println(s"Step root: $scanPointer, free: $freePointer")
      collectDump
    }
    //      println("-----------------------------------------------")
    while(scanPointer != freePointer){
      assert(scanPointer <= freePointer, s"scanPointer $scanPointer > freePointer $freePointer")
      //        println(s"Step scan: $scanPointer, free: $freePointer")
      collectDump
      if (scanPointer.isObj){
        val obj = scanPointer.obj

        //          println("Scanning Obj length = "+length)

        for{
          (x, i) <- obj.cls.fieldList.zipWithIndex
          if x.desc.isRef
        }{
          val (newRoot, nfp) = blit(freePointer, obj.members(i))
          obj.members(i) = newRoot
          freePointer = nfp
        }

        scanPointer += obj.heapSize
      }else{ // it's an Arr
      val arr = scanPointer.arr
        val length = arr.heapSize
        //          println("Scanning Arr length = "+length)

        if (arr.innerType.isRef){
          for (i <- 0 until arr.length){
            val (newRoot, nfp) = blit(freePointer, arr(i))
            println(arr.address)
            arr(i) = newRoot
            freePointer = nfp
          }
        }
        scanPointer += length
      }
    }



    if (from == 0) start = memorySize
    else start = 0

    //      println("==================Collectiong Compelete====================")
    //      println(dump())
    //System.exit(0)
  }
}
