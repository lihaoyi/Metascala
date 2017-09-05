package metascala

import org.objectweb.asm.tree.MethodNode
import metascala.opcodes._
import metascala.rt.Frame
import metascala.util.{Agg, Util}

import scala.collection.mutable
object ColorLogger{
  val sigHandler: PartialFunction[Any, pprint.Tree] = {
    case s: imm.Sig => pprint.Tree.Lazy(_ => Iterator(s.toString))
    case a: Agg[_] =>
      pprint.Tree.Apply(
        "Agg",
        // Ideally we would be able to "recurse" into the children while
        // preserving all the indentation/configuration of the original print
        // call. PPrint provides no easy way to do that right now
        a.toIterator.map(x => pprinter.treeify(x))
      )
  }
  val pprinter = pprint.PPrinter.Color.copy(additionalHandlers =
    sigHandler orElse pprint.PPrinter.Color.additionalHandlers
  )

  def printTrace(trace: Seq[StackTraceElement]) = {
    for(frame <- trace){
      println(fansi.Str.join(
        "    ",
        fansi.Color.Red(frame.getClassName), ".",
        fansi.Color.Cyan(frame.getMethodName), "(",
        fansi.Color.Green(frame.getFileName), ":",
        fansi.Color.Green(frame.getLineNumber.toString), ")"
      ))
    }

  }
}
trait ColorLogger extends rt.Logger{
  def active = true

  def logStep(indentCount: Int,
              clsName: String,
              frame: Frame,
              node: Insn,
              block: BasicBlock,
              getType: Int => String) = {

//    def printOrNot = clsName == "java.util.concurrent.ConcurrentHashMap" &&
//                     frame.method.sig.name == "<init>"
//    if (clsName.contains("MethodHandles") || clsName.contains("MemberName")) {
    if(false){
      val indent = "    " * indentCount
      val r = Util.reader(frame.locals, 0)
      lazy val localSnapshot =
        block.locals
          .flatMap{
              case LocalType.Ref =>
                val r0 = r()
                Seq(getType(r0) + "#" + r0)
              case x => Seq(x.prettyRead(r)).padTo(x.size, "~")
          }
          .toList

      val output = mutable.Buffer.empty[fansi.Str]
      output.append(indent)
      output.appendAll(
        frame.runningClass.shortName.split('/').flatMap(chunk =>
          Seq(fansi.Color.Cyan(chunk), fansi.Str("/"))
        )
      )

      output.append(fansi.Color.Cyan(frame.method.sig.shortName))
      output.append(":")
      output.append(fansi.Color.Green(block.lines(frame.pc._2).toString))
      output.append("  [")
      localSnapshot.map(fansi.Color.Blue(_)) match{
        case Nil =>
        case head :: tail =>
          output.append(head)
          output.appendAll(tail.flatMap(Seq(fansi.Str(", "), _)))
      }
      output.append("]")
      output.append("\n")
      output.append(indent)
      output.appendAll(ColorLogger.pprinter.tokenize(frame.pc, width = 320))
      output.append("  ")
      output.appendAll(ColorLogger.pprinter.tokenize(node, width = 320))
      println(fansi.Str.join(output:_*))
      println()
      //      println(indent + "::\t" + vm.heap.dump().replace("\n", "\n" + indent + "::\t"))
    }
  }

  def logPhi(indentCount: Int, clsName: String, frame: Frame, shifts: Iterator[(Int, Int)]) = {
//    def printOrNot = clsName == "java.util.concurrent.ConcurrentHashMap" &&
//      frame.method.sig.name == "<init>"

    if (false) {
      val indent = "    " * indentCount
      val output = mutable.Buffer.empty[fansi.Str]
      output.append(indent)
      output.append(fansi.Color.Magenta("doPhi"))
      output.append("  ")
      output.append("{")
      var first = true
      for ((src, dest) <- shifts) {
        if (!first) {
          output.append(", ")
        }
        first = false
        output.append(
          fansi.Color.Green(src.toString),
          " -> ",
          fansi.Color.Green(dest.toString)
        )
      }
      output.append("}")
      println(fansi.Str.join(output: _*))
      println()
    }
  }

  def logBasicBlocks(clsName: String,
                     method: MethodNode,
                     basicBlocks: TraversableOnce[BasicBlock],
                     blockBufferThrees: Agg[mutable.Map[Box, Int]],
                     tryCatchBlocks: Agg[TryCatchBlock]) = {
    //    def printOrNot = clsName == "java.util.concurrent.ConcurrentHashMap" &&
    //      frame.method.sig.name == "<init>"
    if (false) {
      def flatten[T](x: TraversableOnce[T]): String = x.mkString("[", ", ", "]")
      def arrow(x: (Any, Any)): String = fansi.Color.Green(x._1.toString) + " -> " + fansi.Color.Green(x._2.toString)
      val output = mutable.Buffer.empty[fansi.Str]
      output.append(
        fansi.Color.Magenta("=" * 20 + clsName + "#" + method.name + "=" * 20),
        "\n\b"
      )
      for(t <- tryCatchBlocks){
        output.appendAll(ColorLogger.pprinter.tokenize(t))
        output.append("\n")
      }
      output.append("\n")
      for ((block, i) <- basicBlocks.toArray.zipWithIndex){

        output.append(fansi.Color.Cyan(i.toString.padTo(8, ' ')), "{")
        var first = true
        for((phi, phiIndex) <- block.phi.zipWithIndex if phi.nonEmpty){
          if (!first) output.append(", ")
          first = false
          output.append(fansi.Color.Green(phiIndex.toString), ": [")
          var first1 = true
          for ((dest, src) <- phi.zipWithIndex if src != -1){
            if (!first1) output.append(", ")
            output.append(arrow((src, dest)))
            first1 = false
          }
          output.append("]")
        }
        output.append("}\n")


        val values = blockBufferThrees(i).values
        val boxes = new Array[Box](if (values.nonEmpty) values.max + 1 else 1)
        for((k, v) <- blockBufferThrees(i)){
          boxes(v) = k
        }
        output.append("        ", boxes.mkString("[", ", ", "]"), "\n")
        for(i <- 0 until block.insns.length){
          output.append(
            "        ",
            fansi.Color.Green(block.lines(i).toString.padTo(8, ' '))
          )
          output.appendAll(ColorLogger.pprinter.tokenize(block.insns(i), width = 320))
          output.append("\n")
        }
        output.append("\n")
      }
      output.append("\n")
      println(fansi.Str.join(output:_*))
    }
  }

  def logException(cls: imm.Type.Cls, msg: String, trace: Seq[StackTraceElement]): Unit = {
    if (false){
      println(fansi.Color.Red(cls.javaName) + ": " + fansi.Color.Yellow(""+msg))
      ColorLogger.printTrace(trace)
    }
  }
}

