package metascala

/**
 * This metascala contains the code involved in reading .class files
 * (using ASM) and generating an immutable representation of all the data
 * structures encoded in the class file.
 *
 * Almost every class in this metascala will have a read() method, which takes
 * in some data structure (provided by ASM) and constructs an instance of the
 * class. read() is generally called recursively to construct the members of
 * each instance, until the entire instance is constructed.
 */
package object imm {

}
