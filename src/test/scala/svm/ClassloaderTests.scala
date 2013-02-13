package svm

import io.Source
import java.io._
import java.net.URI
import javax.tools._
import javax.tools.JavaFileManager.Location
import javax.tools.JavaFileObject.Kind
import org.scalatest.FreeSpec
import parsing.AttributeInfo.{Code, SourceFile}
import scala.collection.JavaConverters._
import java.nio.ByteBuffer
import parsing.ConstantInfo.Utf8
import parsing.{Access, ClassFile, ConstantInfo, MethodInfo}
import ConstantInfo.{Class => ConstClass}
class ClassloaderTests extends FreeSpec{
  object Cp{ def unapply(index: Short)(implicit cp: Seq[ConstantInfo]): Option[ConstantInfo] = {
    cp.lift(index)
  }}
  val CafeBabe =  0xcafebabe

  object Print{ def unapply[A](thing: A): Option[A] = {
    println(thing)
    Some(thing)
  }}

  "hello world" in {
    val files = compile("helloworld")
    val classData = ClassFile.read(ByteBuffer.wrap(files("helloworld.HelloWorld")))
    implicit val constantPool = classData.constant_pool
    val AccessFlags = (Access.Super | Access.Public)
    val ClassFile(
      CafeBabe,  // magic
      0,  // minor_version
      51, // major_version
      _,  // constant_pool
      AccessFlags,  // access_flags
      Cp(ConstClass(Cp(Utf8("helloworld/HelloWorld")))),  // this_class
      Cp(ConstClass(Cp(Utf8("java/lang/Object")))),  // super_class
      Seq(),  // interfaces
      Seq(),  // fields
      Seq(
        MethodInfo(_,
          Cp(Utf8("<init>")),
          Cp(Utf8("()V")),
          Seq(Code(_, _, initBytes, _, _))
        ),
        MethodInfo(_,
          Cp(Utf8("main")),
          Cp(Utf8("([Ljava/lang/String;)V")),
          Seq(Code(_, _, mainBytes, _, _))
        )
      ),  // methods
      Seq(SourceFile(Cp(Utf8("HelloWorld.java"))))   // attributes
    ) = classData
  }





  def compile(path: String): Map[String, Array[Byte]] = {
    val compiler = ToolProvider.getSystemJavaCompiler
    def recursiveListFiles(f: File): Array[File] = {
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }

    object ClassFileManager extends ForwardingJavaFileManager[StandardJavaFileManager](compiler.getStandardFileManager(null, null, null)) {
      private[this] var mapping = Map.empty[String, MemClassFile]
      override def getJavaFileForOutput(location: Location,
                                        className: String,
                                        kind: Kind,
                                        sibling: FileObject)
                                        : JavaFileObject = {

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
