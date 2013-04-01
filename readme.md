ScalaMachine
============

ScalaMachine is a tiny [metacircular]() [Java Virtual Machine (JVM)](1) written in [Scala](). ScalaMachine is less than 3000 lines of plain Scala, while being complete enough that it is able to interpret itself metacircularly (and rather slowly!). Being written in Scala and compiled to [Java bytecode](), the ScalaMachine JVM requires a host JVM in order to run.

The point of ScalaMachine is to have a toy platform to experiment with the JVM: a 3000 Lines of Code (LOC) JVM written in Scala is probably much more approachable than the 250,000 lines of C/C++ which make up [HotSpot](http://openjdk.java.net/groups/hotspot/), the standard implementation. Implementing [continuations]()? You could be done in an afternoon! [Isolates]()? [Threadlocal Heaps]()? [Value Classes]()? Each of these would probably take less than 50 LOC to implement.

Getting Started
---------------
ScalaMachine requires [Scala 2.10]() and is built using [SBT 12](). After checking out the repository, if you have SBT installed, all you need to do is run 

```
sbt test
```

Which will run download the dependencies (currently just [asm]()), compile the code, and run the unit tests in the [test/scala/features]() folder. Compiling ScalaMachine could take up to a minute or two, but running the unit tests should take less than 10 seconds. These tests exercise individual pieces of functionality available on the JVM: math, methods, classes, exceptions, etc., and verify that the result of executing a method via ScalaMachine is identical to the result of executing it directly via [reflection]().

Implementation
--------------
ScalaMachine is a simple Scala application, and compiles to Java bytecode like any other Scala program. It is literally a program that loads in a class file, parses it into a nice to use data structure and then has a `while(true)` loop that interprets the bytecodes one by one, updating the internal state of the VM following the [JVM Spec]() to and spitting out an answer at the end.

In fact, each ScalaMachine JVM is a single Java object, containing in itself all state relevant to its own computation, and instantiating one and invoking methods using it is simple: 

```
val x = new sm.VM()
x.invoke("sm.features.arrays.ArrayStuff", "bubbleSort", Seq(Array(6, 5, 2, 7, 3, 4, 9, 1, 8)))
// Array(1, 2, 3, 4, 5, 6, 7, 8, 9)
```

One thing of note is that ScalaMachine does not have its own memory allocator or [garbage collector](). Rather, it represents virtual objects and arrays as real instances of the classes `vrt.Obj`s and `vrt.Arr`s. References between virtual objects are also references between the real `vrt.Obj` instances. Thus, the garbage collector of the underlying runtime is used to garbage-collect the unreachable objects the inside the ScalaMachine JVM.

Architecture
------------

Apart from the test folder, the main packages of interest in ScalaMachine are:

- [sm/imm](): an immutable model of the data structures that make up a java .class file. These are an almost direct conversion of the data structures provided by the [ASM Tree API](), converted to idiomatic, immutable Scala case classes. These classes should be purely immutable, and should have no dependency on the rest of ScalaMachine.
- [sm/opcodes](): contains the attributes and behavior of the 200ish java bytecodes. Many of the 200 opcodes are unused (as ASM automatically collapses these into other opcodes during parsing), and there are additional opcodes in [Optimized.scala]() which are versions of the default opcodes optimized for the individual ScalaMachine.
- [sm/vrt](): virtual versions of the standard JVM data types: objects, arrays, `int`s, `double`s and the rest of the primitive types. These virtual values populate the heap, operand stack, local variable table and anywhere where variables may be stored within the ScalaMachine VM.
- [sm/rt](): runtime data-structures that make up the JVM: threads, classes, etc. These classes also contain the mutable state associated with these constructs (e.g. static class fields) or ScalaMachine-specific optimizations (e.g. virtual-method vtables) that can't be placed in the [sm/imm]() package.

Many concepts have classes in several of these packages representing them. For example, the abstract idea of a Java "Class" is modelled by:

- `imm.Cls`: the immutable, VM-independent representation of a class parsed from a class file
- `imm.Type.Cls`: a Type representing a Class signature. This contains the qualified name of the class (e.g. `"java.lang.String"`), which may or may not exist, and is also immutable
- `rt.Cls`: the runtime representation of a class, with its mutable state (static fields) and VM-specific optimizations (e.g. vtables)
- `vrt.Cls`: an instance of `vrt.Obj`, representing a "Class" object inside the ScalaMachine JVM, that the interpreted code can manipulate

These types are always referred to by their qualified names in the source code (i.e. `imm.Cls` rather than simply `Cls`) in order to avoid confusion between them or name collisions.

Compatibility
-------------
ScalaMachine implements a subset of the [Java Virtual Machine Specification](). The implementation has been mostly focused on the features that ScalaMachine needs to run. ScalaMachine itself is a medium sized Scala application (almost 3000LOC), making extensive use of the Scala standard library, using the [ASM]() java library for dealing with the `.class` files. However, ScalaMachine does not require (and hence does not implement) several pretty basic things such as:

- **Multiple Threads**
- **Custom ClassLoaders**
- **Enforcement of Access-Control modifiers**

Apart from the language specification, there is a large amount of functionality in the JVM which is from  *native* methods. These are required for the JVM to interact with the outside world in any way, and again ScalaMachine only implements those which were necessary to interpret itself, leaving out a lot of other basic things such as 

- **Filesystem Access**
- **Network Access**
- **System.out.println** (`scala.Predef.println` works though)

Nonetheless, as we'll see, ScalaMachine is compatible enough to interpret itself: a moderately sized Scala program which makes heavy use of the standard library and a small number of external Java libraries.

Metainterpretation
------------------
Despite leaving out all this functionality, ScalaMachine is perfectly capable of loading and interpreting itself! Simply type:

```
sbt test meta
```

into the command line and SBT will run a selection of the unit tests with one level of indirection: rather than directly loading and interpreting the bytecode of the `sqrtFinder` method, for example, this will *load and interpret the bytecode of a method which creates a ScalaMachine interpreter which is then used to interpret `sqrtFinder`*. These tests take significantly longer than the unit tests, and typically take on the order of 30 seconds to a minute to complete.

If you're feeling particularly adventurous, you can try

```
sbt test double-meta
```

which will run a simple unit test with *yet another layer of interpretation*: instead of interpreting the bytecode of a method, or even interpreting an interpreter which interprets the bytecode of a method (as in the case above), this will *interpret an interpreter which interprets an interpreter interpreting the bytecode of a method*. This takes on the order of **3 hours** to complete.

Performance
-----------
The performance of ScalaMachine is absolutely abyssmal: it performs basic optimizations (e.g. pre-calculating virtual-dispatch tables and maintaining invoke-interface caches), and I've tried to eliminate most of the micro-bottlenecks using [JProfiler]() but the performance remains extremely low. 

Part of this arises from the fact that it is written in Scala in a mostly immutable, functional style, and that results in overhead (lots of extra allocations and method calls) over an imperative, mutable style. The other part is probably a fundamental limitation of it being an interpreter, and any major increase in performance would require pretty fundamental changes to the system.

Future Work
-----------
Ideas for where this could go include:

- Fleshing out the completeness of the Java implementation: multiple Threads, ClassLoaders, Filesystem access, enough to run some [standard Java benchmarks]() and applications like the [Scala Compiler]()
- Implement a custom [Heap](), with memory allocator and garbage collector, to remove a dependency on the host JVM
- Implement a [Just-In-Time]()/[Ahead-Of-Time]() compiler to avoid interpretation and speed up execution of the ScalaMachine VM. This could either target bytecode (and run on the host JVM) or LLVM/C/x86 and run on the bare metal.
- Make the ScalaMachine VM self-hosted, such that it can bootstrap itself and run natively without a host JVM