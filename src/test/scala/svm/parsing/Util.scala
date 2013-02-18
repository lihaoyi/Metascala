package svm.parsing

import org.objectweb.asm.{ClassReader, MethodVisitor, ClassVisitor}
import org.objectweb.asm.commons.InstructionAdapter
import org.objectweb.asm.util.TraceClassVisitor
import java.io.{OutputStreamWriter, ByteArrayOutputStream, File, PrintWriter}
import javax.tools._
import javax.tools.JavaFileManager.Location
import javax.tools.JavaFileObject.Kind
import scala.collection.JavaConverters._
import java.net.URI
import io.Source

object Util{

  object Print{ def unapply[A](thing: A): Option[A] = {
    println(thing)
    Some(thing)
  }}


  def printClass(data: Array[Byte]) = {
    val r = new ClassReader(data)
    val tracer = new TraceClassVisitor(new PrintWriter(System.out))
    r.accept(tracer, 0)
  }
  class MethodPrinterVisitor(api: Int) extends ClassVisitor(api){
    override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) ={
      println("\n"+name+desc);
      new InstructionAdapter(new MethodVisitor(4) {}){
        override def visitInsn(opcode: Int) {
          println(opcode);
          super.visitInsn(opcode);
        }
      }
    }
  }


  def compile(path: String): Map[String, Array[Byte]] = {
    val compiler = ToolProvider.getSystemJavaCompiler
    def recursiveListFiles(f: File): Array[File] = {
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }

    object ClassFileManager
      extends ForwardingJavaFileManager[StandardJavaFileManager](compiler.getStandardFileManager(null, null, null)) {

      private[this] var mapping = Map.empty[String, MemClassFile]
      override def getJavaFileForOutput(location: Location,
                                        className: String,
                                        kind: Kind,
                                        sibling: FileObject): JavaFileObject = {

        val newFile = new MemClassFile(className, kind);
        mapping = mapping + (className -> newFile)
        newFile
      }
      def finish = mapping.mapValues(_.getBytes())
    }
    class MemClassFile(name: String, kind: Kind)
      extends SimpleJavaFileObject(URI.create("string:///" + name.replace('.', '/') + kind.extension), kind) {
      val bos = new ByteArrayOutputStream();
      def getBytes() = bos.toByteArray();
      override def openOutputStream() = bos
    }
    class MemSourceFile(name: String, code: String)
      extends SimpleJavaFileObject(URI.create("string:///" + name),Kind.SOURCE) {
      override def getCharContent(ignoreEncodingErrors: Boolean) = code
    }


    val diagnostics = new DiagnosticCollector[JavaFileObject]

    val compilationUnits =
      recursiveListFiles(new File(s"src/test/resources/$path"))
        .map{f => new MemSourceFile(f.getName(), Source.fromFile(f).getLines().mkString("\n"))}


    val task = compiler.getTask(new OutputStreamWriter(System.out), ClassFileManager, diagnostics, null, null, compilationUnits.toIterable.asJava)
    task.call()
    ClassFileManager.finish
  }

}
