Metascala
============

Metascala is a tiny [metacircular](http://en.wikipedia.org/wiki/Metacircular) [Java Virtual Machine (JVM)](http://en.wikipedia.org/wiki/Jvm) written in the [Scala](http://en.wikipedia.org/wiki/Scala_(programming_language)) programming language. Metascala is less than 3000 lines of plain Scala, while being complete enough that it is able to interpret itself metacircularly (and rather slowly!). Being written in Scala and compiled to [Java bytecode](http://en.wikipedia.org/wiki/Java_bytecode), the Metascala JVM requires a host JVM in order to run.

The point of Metascala is to have a toy platform to experiment with the JVM: a 3000 Lines of Code (LOC) JVM written in Scala is probably much more approachable than the 250,000 lines of C/C++ which make up [HotSpot](http://openjdk.java.net/groups/hotspot/), the standard implementation. Implementing [continuations](http://en.wikipedia.org/wiki/Continuation)? You could be done in an afternoon! [Isolates](http://www.javalobby.org/java/forums/t105978.html)? [Threadlocal Heaps](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.2.7362)? [Value Classes](https://blogs.oracle.com/jrose/entry/value_types_in_the_vm)? Each of these would probably take less than 50 LOC to implement.

Getting Started
---------------
Metascala requires [Scala 2.10](http://www.scala-lang.org/downloads) and is built using [SBT 12](http://www.scala-sbt.org/). After checking out the repository, if you have SBT installed, all you need to do is run

```
sbt
> test-only metascala.features.*
```

Which will run download the dependencies (currently just [asm](http://asm.ow2.org/)), compile the code, and run the unit tests in the [test/scala/features](test/scala/features) folder. Compiling Metascala could take up to a minute or two, but running the unit tests should take less than 10 seconds. These tests exercise individual pieces of functionality available on the JVM: math, methods, classes, exceptions, etc., and verify that the result of executing a method via Metascala is identical to the result of executing it directly via [reflection](http://docs.oracle.com/javase/tutorial/reflect/).

Implementation
--------------
Metascala is a simple Scala application, and compiles to Java bytecode like any other Scala program. It is literally a program that loads in a class file, parses it into a data structure and then has a `while(true)` loop that interprets the bytecodes one by one, updating the internal state of the VM following the [JVM Spec](http://docs.oracle.com/javase/specs/jvms/se7/html/) and spitting out an answer at the end.

In fact, each Metascala JVM is a single Java object, containing in itself all state relevant to its own computation, and instantiating one and invoking methods using it is simple: 

```scala
val x = new metascala.VM()
x.invoke("metascala.features.arrays.ArrayStuff", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
// Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
```

One thing of note is that Metascala does not have its own memory allocator or [garbage collector](http://en.wikipedia.org/wiki/Garbage_collection_(computer_science)). Rather, it represents virtual objects and arrays as real instances of the classes `vrt.Obj`s and `vrt.Arr`s. References between virtual objects are also references between the real `vrt.Obj` instances. Thus, the garbage collector of the underlying runtime is used to garbage-collect the unreachable objects the inside the Metascala JVM.

Architecture
------------

Apart from the test folder, the main packages of interest in Metascala are:

- [metascala/imm](src/main/scala/metascala/imm): an immutable model of the data structures that make up a java .class file. These are an almost direct conversion of the data structures provided by the [ASM Tree API](http://www.geekyarticles.com/2011/10/manipulating-java-class-files-with-asm_13.html), converted to idiomatic, immutable Scala case classes. These classes should be purely immutable, and should have no dependency on the rest of Metascala.
- [metascala/opcodes](src/main/scala/metascala/opcodes): contains the attributes and behavior of the 200ish java bytecodes. Many of the 200 opcodes are unused (as ASM automatically collapses these into other opcodes during parsing), and there are additional opcodes in [Optimized.scala](src/main/scala/metascala/opcodes/Optimized.scala) which are versions of the default opcodes optimized for the individual Metascala.
- [metascala/vrt](src/main/scala/metascala/vrt): virtual versions of the standard JVM data types: objects, arrays, `int`s, `double`s and the rest of the primitive types. These virtual values populate the heap, operand stack, local variable table and anywhere where variables may be stored within the Metascala VM.
- [metascala/rt](src/main/scala/metascala/rt): runtime data-structures that make up the JVM: threads, classes, etc. These classes also contain the mutable state associated with these constructs (e.g. static class fields) or Metascala-specific optimizations (e.g. virtual-method vtables) that can't be placed in the [metascala/imm](src/main/scala/metascala/imm) package.
- [metascala/natives](src/main/scala/metascala/natives): contains trapped methods, or [intrinsics](http://en.wikipedia.org/wiki/Intrinsic_function) which when called within the Metascala VM result in some interaction with the Host VM. There is a default implementation of Bindings, but it an easily be swapped out for a custom version of Bindings e.g. to redirect filesystem access, or mock out `currentTimeMillis()` with a custom time.

Many concepts have classes in several of these packages representing them. For example, the abstract idea of a Java "Class" is modelled by:

- `imm.Cls`: the immutable, VM-independent representation of a class parsed from a class file
- `imm.Type.Cls`: a Type representing a Class signature. This contains the qualified name of the class (e.g. `"java.lang.String"`), which may or may not exist, and is also immutable
- `rt.Cls`: the runtime representation of a class, with its mutable state (static fields) and VM-specific optimizations (e.g. vtables)
- `vrt.Cls`: an instance of `vrt.Obj`, representing a "Class" object inside the Metascala JVM, that the interpreted code can manipulate

These types are always referred to by their qualified names in the source code (i.e. `imm.Cls` rather than simply `Cls`) in order to avoid confusion between them or name collisions.


Compatibility
-------------
Metascala implements a subset of the [Java Virtual Machine Specification](http://docs.oracle.com/javase/specs/jvms/se7/html/). The implementation has been mostly focused on the features that Metascala needs to run. However, Metascala does not require (and hence does not implement) several pretty basic things such as:

- **Multiple Threads**
- **Custom ClassLoaders**
- **Enforcement of Access-Control modifiers**

Apart from the language specification, there is a large amount of functionality in the JVM which is from  *native* methods. These are required for the JVM to interact with the outside world in any way, and again Metascala only implements those which were necessary to interpret itself, leaving out a lot of other basic things such as 

- **Filesystem Access**
- **Network Access**
- **System.out.println** (`scala.Predef.println` works though)

Nonetheless, as we'll see, Metascala is compatible enough to interpret itself: a moderately sized Scala program which makes heavy use of the standard library, some basic reflection, and a small number of external Java libraries.

MetaScala has been tested on Windows 7 using the Sun JVM (Java 7), and Ubuntu 12.04 using OpenJDK 7.

Metainterpretation
------------------
Despite leaving out all this functionality, Metascala is perfectly capable of loading and interpreting itself! Simply enter:

```
sbt
> test-only metascala.full.*
```

into the command line and SBT will run a selection of the unit tests with one level of indirection: rather than directly loading and interpreting the bytecode of the `sqrtFinder` method, for example, this will *load and interpret the bytecode of a method which creates a Metascala interpreter which is then used to interpret `sqrtFinder`. These tests take significantly longer than the unit tests, and typically take on the order of about30 seconds to complete.



Performance
-----------
The performance of Metascala is absolutely abyssmal: it performs basic optimizations (e.g. pre-calculating virtual-dispatch tables and maintaining invoke-interface caches), and I've tried to eliminate most of the micro-bottlenecks using [JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html) but the performance remains extremely low.

Part of this arises from the fact that it is written in Scala in a mostly immutable, functional style, and that results in overhead (lots of extra allocations and method calls) over an imperative, mutable style. The other part is probably a fundamental limitation of it being an interpreter, and any major increase in performance would require pretty fundamental changes to the system.

Future Work
-----------
Ideas for where this could go include:

- Fleshing out the completeness of the Java implementation: multiple Threads, ClassLoaders, Filesystem access, enough to run some standard Java benchmarks and applications like the [Scala Compiler](https://github.com/scala/scala)
- Get reflection working for real
- Implement a custom [Heap](http://en.wikipedia.org/wiki/Heap_(programming)), with memory allocator and garbage collector, to remove a dependency on the host JVM
- Implement a Just-In-Time/Ahead-Of-Time compiler to avoid interpretation and speed up execution of the Metascala VM. This could either target bytecode (and run on the host JVM) or LLVM/C/x86 and run on the bare metal.
- Make the Metascala VM self-hosted, such that it can bootstrap itself and run natively without a host JVM

Feel free to contact me (below) or open an issue/send a pull request if you're interested and want to help out. Contributions are welcome!

Fun Facts
---------

- At only 3000 lines of source code, Metascala is probably one of the smallest JVMs ever.
- Metascala took about a months worth of a single person's free time to construct.
- Metascala isn't a metacircular Java/Scala interpreter, because it is currently unable to interpret the Java/Scala compilers.
- The number of native method bindings to the JVM is huge, and unlike the virtual machine specification, completely undocumented, although it is necessary to run basically anything. The only way to find out what natives are missing is to run stuff and see it crash when it encounters a missing native method.
- The 90kb of source code gets compiled into 1800kb of binaries, an increase of 20x.

Credits
-------
Metascala was made by Li Haoyi (haoyi.sg at gmail.com), and heavily inspired by [Doppio](https://github.com/int3/doppio)

Copyright (c) 2013, Li Haoyi (haoyi.sg at gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.