package metascala

import metascala.imm.Type.Prim
import scala.collection.mutable
class Heap(memorySize: Int,
           getRoots: () => Seq[ArrRef],
           getLinks: (Int, Int) => Seq[Int]){

  val memory = new Array[Int](memorySize * 2)
  var start = 0
  var freePointer = 1

  def allocate(n: Int) = {
    if (freePointer + n > memorySize + start) {
      println("COLLECT LOL")
      collect(start)
      if (freePointer + n > memorySize + start) {
        throw new Exception("Out of Memory!")
      }
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
    if (src == 0 || memory(src) == 0) {
      (0, freePointer)
    } else if (memory(src) == 0){
      throw new Exception("Can't point to nothing! " + src + " -> " + memory(src))
    }else if (memory(src + 1) >= 0){
      val headerSize = if (isObj(memory(src))) rt.Obj.headerSize else rt.Arr.headerSize
      val length =  memory(src+1) + headerSize

      //        println(s"blit obj length: ${obj.heapSize}")
      System.arraycopy(memory, src, memory, freePointer, length)
      memory(src + 1) = -freePointer
      (freePointer, freePointer + length)


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
    //vm.threads(0).threadStack.map(x => x.runningClass.name + "/" + x.method.sig + "\t" + x.method.code.blocks(x.pc._1).insns(x.pc._2)).foreach(println)
    println("starting " + (freePointer - from))

    val roots = getRoots()

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

      val links = getLinks(memory(scanPointer), memory(scanPointer+1))
      val length = memory(scanPointer + 1) + rt.Obj.headerSize

      for(i <- links){
        val (newRoot, nfp) = blit(freePointer, memory(scanPointer + i))
        memory(scanPointer + i) = newRoot
        freePointer = nfp
      }

      scanPointer += length
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
