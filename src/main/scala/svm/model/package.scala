package svm
import collection.convert.wrapAsScala._
/**
 * Created with IntelliJ IDEA.
 * User: Haoyi
 * Date: 2/20/13
 * Time: 3:08 AM
 * To change this template use File | Settings | File Templates.
 */
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
