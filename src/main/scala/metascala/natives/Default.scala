package metascala
package natives


import java.io.DataInputStream
import vrt.Arr

trait Default extends Bindings{

  val fileLoader = (name: String) => {
    val slashName = s"/$name"

    val loaded = getClass.getResourceAsStream(slashName)
    if (loaded == null) None
    else {
      val stream = new DataInputStream(loaded)
      val bytes = new Array[Byte](stream.available())
      stream.readFully(bytes)
      Some(bytes)
    }
  }

  val properties = Map[String, String](
    /*"java.home" -> "C ->/java_home",
    "sun.boot.class.path" -> "C ->/classes",
    "file.encoding" ->"US_ASCII",
    "java.ext.dirs" -> "",
    "java.vendor" ->"Doppio",
    "java.version" -> "1.6",
    "java.vendor.url" -> "https ->//github.com/int3/doppio",
    "java.class.version" -> "50.0",
    "java.security.debug" -> "access,failure",
    "java.security.auth.debug" -> "access,failure",
    "java.specification.version" -> "1.6",
    "jdk.map.althashing.threshold" -> "-1",
    "line.separator" ->"\n",
    "file.separator" ->"/",
    "path.separator" ->":",
    "user.dir" -> ".",
    "user.home" ->".",
    "user.name" ->"DoppioUser",
    "os.name" ->"doppio",
    "os.arch" -> "js",
    "os.version" -> "0"*/

  )


  val trapped = {
    Seq(
      "java"/(
        "lang"/(
          "Class"/(
            "getClassLoader0()Ljava/lang/ClassLoader;" x value(I)(0, 0),
            "desiredAssertionStatus0(Ljava/lang/Class;)Z" x value(I)(0, 0),
            "registerNatives()V" x noOp(0)
            ),
          "Object"/(
            "registerNatives()V" x noOp(0)
          ),
          "System"/(
            "arraycopy(Ljava/lang/Object;ILjava/lang/Object;II)V" x { vt =>
              val (src, srcIndex, dest, destIndex, length) = (vt.pop, vt.pop, vt.pop, vt.pop, vt.pop)
              System.arraycopy(vt.vm.Heap, src + srcIndex + 2, vt.vm.Heap, dest + destIndex + 2, length)

            },
            "nanoTime()J" x value(J)(0, System.nanoTime()),
            "currentTimeMillis()J" x value(J)(0, System.currentTimeMillis())
          ),
          "String"/(
            "<clinit>()V" x noOp(0)
          ),
          "System"/(
            "registerNatives()V" x noOp(0)
          )
        )
      )
      /*"scala"/(
        "Predef$"/(
          "println(Ljava/lang/String;)V" x2 {
            vt => (x: vrt.Obj, y: vrt.Obj) =>
              import vt.vm
              println("VIRTUAL " + vrt.unvirtString(y))
          },
          "println(Ljava/lang/Object;)V" x2 {
            vt => (x: vrt.Obj, y: vrt.Obj) =>
              import vt.vm
              println("VIRTUAL " + vrt.unvirtString(y))
          }

        )
      )*/

    ).toRoute()
  }

}

