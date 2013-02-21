package svm
import collection.convert.wrapAsScala._
package object model {
  implicit class c[T](list: java.util.List[T]){
    def safeList: List[T] = {
      Option(list).toList.flatten
    }
  }
  implicit class a[T](list: Array[T]){
    def safeList: List[T] = {
      Option(list).toList.flatten
    }
  }
  implicit class o[T](a: T){
    def safeOpt: Option[T] = Option(a)
  }
}
