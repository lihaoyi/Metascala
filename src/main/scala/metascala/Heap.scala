package metascala

import metascala.imm.Type.Prim

class Heap(memorySize: Int)(implicit vm: VM){

  val memory = new Array[Int](memorySize * 2)
  var start = 0
  var freePointer = 1

  def allocate(n: Int) = {

    if (freePointer + n > memorySize + start) {
      println("COLLECT LOL")
      collect(start)
    }
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
//    println(s"blit free:$freePointer, src:$src")
    if (src == 0) {
      (0, freePointer)
    } else if (memory(src + 1) >= 0){
      if (src.isObj) {
        val obj = src.obj
//        println(s"blit obj length: ${obj.heapSize}")
        System.arraycopy(memory, src, memory, freePointer, obj.heapSize)
        memory(src + 1) = -freePointer
        (freePointer, freePointer + obj.heapSize)
      } else if (src.isArr){ // it's an Arr

        val length = src.arr.heapSize
//        println(s"blit arr length: $length")
        System.arraycopy(memory, src, memory, freePointer, length)
        memory(src + 1) = -freePointer
        (freePointer, freePointer + length)
      }else{
        (0, freePointer)
      }
    }else{
//      println(s"non-blitting $src -> ${-memory(src + 1)}")
      (-memory(src + 1), freePointer)
    }
  }
  def collect(from: Int){
    val to = (from + memorySize) % (2 * memorySize)
    for(i <- to until (to+memorySize)){
      memory(i) = 0
    }
    println("===============Collecting==================")
    println("arrayTypeCache " + vm.arrayTypeCache.length)
    println("internedStrings " + vm.internedStrings.size)
    println("typeObjCache " + vm.internedStrings.size)
    //vm.threads(0).threadStack.map(x => x.runningClass.name + "/" + x.method.sig + "\t" + x.method.code.blocks(x.pc._1).insns(x.pc._2)).foreach(println)
    println("starting " + (freePointer - from))

    val roots = vm.getRoots

//        println(s"allRoots ${roots.map(_())}")

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

    }

    while(scanPointer != freePointer){

      assert(scanPointer <= freePointer, s"scanPointer $scanPointer > freePointer $freePointer")

      if (scanPointer.isObj){
        val obj = scanPointer.obj

//          println("Scanning Obj length = "+obj.heapSize)

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
//                  println("Scanning Arr length = "+length)

        if (arr.innerType.isRef){
          for (i <- 0 until arr.length){
            val (newRoot, nfp) = blit(freePointer, arr(i))
            arr(i) = newRoot
            freePointer = nfp
          }
        }
        scanPointer += length
      }
    }



    if (from == 0) start = memorySize
    else start = 0
    println("ending " + (freePointer - start))

//    println("==================Collectiong Compelete====================")
//    println(dump())
//    vm.getRoots
    //System.exit(0)
  }
}
