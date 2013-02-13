package svm

import io.Source
import java.io._
import java.net.URI
import javax.tools._
import javax.tools.JavaFileManager.Location
import javax.tools.JavaFileObject.Kind
import org.scalatest.FreeSpec
import scala.collection.JavaConverters._
import java.nio.ByteBuffer
import svm.ClassLoader.ConstantInfo.Utf8
import svm.ClassLoader.{ConstantInfo, MethodInfo}

class ClassloaderTests extends FreeSpec{
  "hello world" in {
    val files = compile("helloworld")
    val classData = ClassLoader.ClassFile.read(ByteBuffer.wrap(files("helloworld.HelloWorld")))
    import classData._
    implicit class withConst(n: Short){
      def const = constant_pool(n)
    }
    assert(java.lang.Integer.toHexString(magic) === "cafebabe")
    val Seq() = interfaces
    val Seq() = fields
    val Utf8("helloworld/HelloWorld") =
        this_class.const
                  .asInstanceOf[ConstantInfo.Class]
                  .name_index
                  .const

    val Utf8("java/lang/Object") =
      super_class.const
        .asInstanceOf[ConstantInfo.Class]
        .name_index
        .const

    val Seq(
      MethodInfo(1, name1, desc1, _),
      MethodInfo(9, name2, desc2, _)
    ) = methods

    val Utf8("<init>") = name1.const
    val Utf8("()V") = desc1.const

    val Utf8("main") = name2.const
    val Utf8("([Ljava/lang/String;)V") = desc2.const

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
