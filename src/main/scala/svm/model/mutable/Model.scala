package svm
package model
package mutable

class Class(classFile: immutable.ClassFile, statics: Map[String, Any] = Map.empty)
class Object(val cls: Class, var members: Map[String, Any] = Map.empty)