package metascala

import metascala.opcodes.{BasicBlock, Insn}
import metascala.rt.Thread.Frame
import metascala.util.Util

import scala.collection.mutable

trait ColorLogger extends rt.Thread.Logger{
  def active = true
  def logStep(indentCount: Int,
              clsName: String,
              frame: Frame,
              node: Insn,
              block: BasicBlock) = {
    val indent = "    " * indentCount
    def printOrNot = true
    if (printOrNot) {

      val r = Util.reader(frame.locals, 0)
      lazy val localSnapshot =
        block.locals
          .flatMap(x => Seq(x.prettyRead(r)).padTo(x.size, "~"))
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
      output.appendAll(pprint.tokenize(frame.pc))
      output.append("  ")
      output.appendAll(pprint.tokenize(node))
      println(fansi.Str.join(output:_*))
      println()
      //      println(indent + "::\t" + vm.heap.dump().replace("\n", "\n" + indent + "::\t"))
    }

  }
  def logPhi(indentCount: Int, clsName: String, shifts: Iterator[(Int, Int)]) = {
    def printOrNot = clsName.contains("AbstractValidatingLambdaMetafactory")

    if (printOrNot) {
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
}

