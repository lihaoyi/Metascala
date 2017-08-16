- Investigate heap corruption `ArrayIndexOutOfBounds: -1` error when running
  some tests in low-interpreter-memory situations, but not low enough to cause
  a proper `Exception("Out of Memory!")` to be thrown
  
- Get `InvokeDynamic` working

- Flesh out `ClassLoader` support to handle multiple classloaders and trees of
  related classloaders

- Get proper stack traces working when something fails in an `invoke` called
  from a native binding

- More complete support for Rhino, with bigger test scripts 

- Get Javac working inside Metascala

- Upgrade to Scala 2.12

- Try to optimize SSA register instructions to reduce number of redundant phi 
  nodes

- Experiment with JIT compilation rather than interpretation

- Implement dense arrays of bytes/shorts/chars, rather than taking up an entire
  4-byte int for each item

- Shift callstack, class, bytecode data structures to binary memory rather than host VM
  objects
  
- Add tests to ensure sorted-ness of DefaultBindings and well-formed-ness of 
  the method signatures
  
- Minimize use of unsafe `VM#arr` and `VM#obj` methods, and convert use cases
  to instead use scoped `VM#alloc`s