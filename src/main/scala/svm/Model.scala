package svm

import model.ClassFile

class Class(classFile: ClassFile, val statics: collection.mutable.Map[String, Any] = collection.mutable.Map.empty)
class Object(val cls: Class, val members: collection.mutable.Map[String, Any] = collection.mutable.Map.empty)