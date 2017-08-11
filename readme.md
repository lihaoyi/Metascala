# Metascala

Metascala is a tiny [metacircular](http://en.wikipedia.org/wiki/Metacircular) [Java Virtual Machine (JVM)](http://en.wikipedia.org/wiki/Jvm) written in the [Scala](http://tinyurl.com/6etjds) programming language. Metascala is barely 3000 lines of Scala, and is complete enough that it is able to interpret itself metacircularly. Being written in Scala and compiled to [Java bytecode](http://en.wikipedia.org/wiki/Java_bytecode), the Metascala JVM requires a host JVM in order to run.

The goal of Metascala is to create a platform to experiment with the JVM: a 3000 line JVM written in Scala is probably much more approachable than the 1,000,000 lines of C/C++ which make up [HotSpot](http://openjdk.java.net/groups/hotspot/), the standard implementation, and more amenable to implementing fun features like [continuations](http://en.wikipedia.org/wiki/Continuation), [isolates](http://www.javalobby.org/java/forums/t105978.html) or [value classes](https://blogs.oracle.com/jrose/entry/value_types_in_the_vm). The 3000 lines of code gives you:

- The bytecode interpreter, together with all the run-time data structures
- A [stack-machine](http://en.wikipedia.org/wiki/Stack_machine) to [SSA](http://en.wikipedia.org/wiki/Static_single_assignment_form) register-machine bytecode translator
- A custom [heap](https://en.wikipedia.org/wiki/Memory_management), complete with a stop-the-world, copying [garbage collector](http://tinyurl.com/d77yltz)
- Implementations of parts of the [JVM's native interface](http://en.wikipedia.org/wiki/Java_Native_Interface)

Although it is far from a complete implementation, Metascala already provides the ability to run untrusted bytecode securely (albeit slowly), since every operation which could potentially cause harm (including memory allocations and CPU usage) is virtualized and can be controlled. [Ongoing work](#ongoing-work) includes tightening of the security guarantees, improving compatibility and increasing performance.

# Getting Started
<!-- --------------- -->
Metascala requires [Scala 2.10](http://www.scala-lang.org/downloads) and is built using [SBT 12](http://www.scala-sbt.org/). After checking out the repository, if you have SBT installed, all you need to do is run

```
sbt
> test-only metascala.features.*
```

Which will download the dependencies (currently just [asm](http://asm.ow2.org/)), compile the code, and run the unit tests in the [test/scala/features](test/scala/features) folder. Compiling Metascala could take a minute, but running the unit tests should take less than 10 seconds. These tests exercise individual pieces of functionality available on the JVM: math, methods, classes, exceptions, etc., and verify that the result of executing a method via Metascala is identical to the result of executing it directly via [reflection](http://docs.oracle.com/javase/tutorial/reflect/).

Apart from the basic feature tests, metascala also includes basic tests for the [garbage collector](src/test/scala/metascala/full/GCTests.scala), [Java](src/test/scala/metascala/full/JavaLibTest.scala) and [Scala](src/test/scala/metascala/full/ScalaLib.scala) library functionality. Lastly, Metascala contains tests for [Meta-interpretation](src/test/scala/metascala/full/MetacircularTest.scala), which tests the ability for Metascala to interpret its own source code, which in turn interprets some simple programs (e.g. a square-root calculator). Meta-interpretation is extremely slow, and these tests take **several minutes** to run.

## Implementation
<!-- --------------- -->
Metascala is a simple Scala application, and compiles to Java bytecode like any other Scala program. It is literally a program that loads in a class file, parses it into a data structure and then has a `while(true)` loop that interprets the bytecodes one by one, updating the internal state of the VM following the [JVM Spec](http://docs.oracle.com/javase/specs/jvms/se7/html/) and spitting out an answer at the end.

In fact, each Metascala JVM is a single Java object, containing in itself all state relevant to its own computation. Instantiating one and invoking methods using it is simple:

```scala
val x = new metascala.VM()
x.invoke("metascala.features.arrays.ArrayStuff", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
// Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
```

Arguments passed into the `invoke()` method are converted from their real representation into virtual versions (the `vrt.*` classes) to be handled within the Metascala VM. The return value is similarly converted from a virtual value back to a real value before being given to the caller of `invoke()`.

The main packages of interest in Metascala are:

### [metascala/imm](src/main/scala/metascala/imm)
An immutable model of the data structures that make up a Java .class file. These are an almost direct conversion of the data structures provided by the [ASM Tree API](http://www.geekyarticles.com/2011/10/manipulating-java-class-files-with-asm_13.html), converted to idiomatic, immutable Scala case classes. These classes should be purely immutable, and should have no dependency on the rest of Metascala.

### [metascala/opcodes](src/main/scala/metascala/opcodes)
This contains the logic related to [parsing and compiling](src/main/scala/metascala/opcodes/Conversion.scala) the Java bytecode instructions from the default hybrid stack/register format into an [SSA bytecode](src/main/scala/metascala/opcodes/Insn.scala), simplifying it in the process. Currently Metascala does not perform any real optimizations on the bytecode apart from linking up class/method names with the relevant classes and methods, but the [SSA](http://en.wikipedia.org/wiki/Static_single_assignment_form) format should make it easier to perform these operations in the future.

### [metascala/rt](src/main/scala/metascala/rt)
Runtime data-structures that make up the JVM: [threads](src/main/scala/metascala/rt/Thread.scala), [classes](src/main/scala/metascala/rt/Cls.scala), [methods](src/main/scala/metascala/rt/Method.scala), [objects and arrays](src/main/scala/metascala/rt/Obj.scala). These classes also contain the mutable state associated with these constructs (e.g. static class fields) or Metascala-specific optimizations (e.g. pre-computed [vtables](http://en.wikipedia.org/wiki/Virtual_method_table)).

### [metascala/natives](src/main/scala/metascala/natives)
Trapped methods, or [Bindings](src/main/scala/metascala/Bindings.scala), which when called within the Metascala VM result in some interaction with the Host VM. There is a [default](src/main/scala/metascala/Default.scala) implementation of bindings, but it can easily be swapped out for a custom version of Bindings e.g. to redirect filesystem access, or mock out `currentTimeMillis()` with a custom time. All interaction between the code inside the VM and the external world takes place through these Bindings.

---------------------------------------------------------

Many concepts have classes in several of these packages representing them. For example, the abstract idea of a Java "Class" is modelled by:

- `imm.Cls`: the immutable, VM-independent representation of a class parsed from a class file
- `imm.Type.Cls`: a Type representing a Class signature. This contains the qualified name of the class (e.g. `"java.lang.String"`), which may or may not exist, and is also immutable
- `rt.Cls`: the runtime representation of a class, with its mutable state (static fields) and VM-specific optimizations (e.g. vtables)

These types are always referred to by their qualified names in the source code (i.e. `imm.Cls` rather than simply `Cls`) in order to avoid confusion between them or name collisions.

## Compatibility
<!-- --------------- -->
Metascala implements a subset of the [Java Virtual Machine Specification](http://docs.oracle.com/javase/specs/jvms/se7/html/). The implementation has been mostly focused on the features that Metascala needs to run. However, Metascala does not require (and hence does not implement) several things such as:

- **Multiple Threads**
- **Custom ClassLoaders**
- **Enforcement of Access-Control modifiers**

Apart from the language specification, there is a large amount of functionality in the JVM which is from  *native* methods. These are required for the JVM to interact with the outside world in any way, and again Metascala only implements those which were necessary to interpret itself, leaving out a lot of other basic things such as 

- **Filesystem Access**
- **Network Access**
- **System.out.println** (`scala.Predef.println` works though!)

Nonetheless, Metascala is compatible enough to interpret itself: a moderately sized Scala program which makes heavy use of the standard library, some basic reflection, and some external Java libraries (Basically only [ASM](http://asm.ow2.org/) right now).

MetaScala has been tested on Windows 7 using the Sun JVM (Java 7), and Ubuntu 12.04 using OpenJDK 7.

## Performance
<!-- --------------- -->
The performance of Metascala is absolutely abysmal: it performs basic optimizations (e.g. pre-calculating virtual-dispatch tables and maintaining invoke-interface caches), but not much more. This is partly because it is written in Scala in a mostly immutable, functional style, and that results in overhead (lots of extra allocations and method calls) over an imperative, mutable style. The other part is probably a fundamental limitation of being an interpreter, and any major increase in performance would require pretty fundamental changes to the system.

This can easily be improved a great deal in the future: micro-bottlenecks (e.g. for-comprehensions) can be optimized, and the SSA bytecode is amenable to analysis and optimization. Nonetheless, my focus so far has been on completeness and compliance; performance optimizations will have to wait.

## Security
<!-- --------------- -->
Part of Metascala's goal is to allow the developer to safely run arbitrary untrusted bytecode. Metascala's approach to security is completely separate from the JVM's existing security model. In short: everything is virtualized, and everything is controlled. Because it reads and interprets each and every bytecode one by one, code executed with the Metascala VM cannot:

- **Loop forever**: Metascala can simply stop the interpretation after a certain amount of bytecodes have been utilized.
- **Allocate unbounded memory**: code run with a Metascala VM runs on its own heap (basically one big byte array), with its own garbage collector. No matter how many allocations it makes, it cannot allocate more memory than the Metascala VM's heap has available.
- **Perform unsafe actions**: by default, all methods or instructions interpreted by the Metascala VM only affect the Metascala VM's internal state, and nothing else. Any attempts to influence the outside world (e.g. by printing debug statements, or loading data from files) has to happen through an explicitly created Bindings object. This single-point-of-entry makes it easy to confidently and completely lock down the untrusted code.

There are still some weaknesses in Metascala's security model: time spent garbage collecting isn't accounted for, neither is memory not directly allocated but required by the VM's auxiliary data structures (e.g. classes). These provide an attacker means to consume more resources than they should be allowed to, and solving this is part of the ongoing work.

## Ongoing Work
<!-- --------------- -->
Immediate work includes:

- Fleshing out the completeness of the Java implementation: multiple Threads, ClassLoaders, Filesystem access, enough to run some standard Java benchmarks and applications like [Rhino Javascript](https://developer.mozilla.org/en/docs/Rhino) or the [Scala Compiler](https://github.com/scala/scala).
- Moving more of the VM's runtime functionality data-structures inside of the Metascala VM, rather than outside. For example, making the garbage collector run inside the VM, or having the class-related data structures inside the VM's heap, would allow better control of the VMs resource usage since they would count toward any bytecode/memory limits imposed on the VM.
- Optimizing performance using macros. e.g. replacing for-loops with [macro-based for-loops](https://github.com/non/spire#syntax), or using macros to pre-compile the [Bindings](src/main/scala/metascala/natives/Bindings.scala) table so the work does not need to be done at run-time. Reducing the start-up cost and run-time overhead would help make Metascala a lightweight container for untrusted code.

Feel free to contact me (below) or open an issue/send a pull request if you're interested and want to help out. Contributions are welcome!

## Fun Facts
<!-- --------------- -->
- At only 3000 lines of source code, Metascala is probably one of the smallest JVMs ever.
- At 60 seconds to compile, it's probably also one of the slowest to compile, compiling at only 50 lines per second.
- Metascala isn't a metacircular Java/Scala interpreter, because it is currently unable to interpret the Java/Scala compilers.
- The number of native method bindings to the JVM is huge, and unlike the virtual machine specification, completely undocumented, although it is necessary to run basically anything. The only way to find out what natives are missing is to run stuff and see it crash when it encounters a missing native method.
- The 90kb of source code gets compiled into 1800kb of binaries, an increase of 20x. Compiled Scala results in a lot of class files.

## Credits
<!-- --------------- -->
Copyright (c) 2013, Li Haoyi (haoyi.sg at gmail.com)

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
