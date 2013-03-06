package svm

import model.Type

object Virtualizer {
  def fromVirtual[T](x: Any): T = {
    def cloneArray[T](x: Array[T]): Array[T] = {
      val newArray = x.clone()
      for(i <- 0 until x.length){
        newArray(i) = fromVirtual[T](newArray(i))
      }
      newArray
    }
    x match{
      case null => null
      case x: Boolean => x
      case x: Byte => x
      case x: Char => x
      case x: Short => x
      case x: Int => x
      case x: Long => x
      case x: Float => x
      case x: Double => x
      case x: Array[Any] => cloneArray(x)
      case x: Array[Boolean] => cloneArray(x)
      case x: Array[Byte] => cloneArray(x)
      case x: Array[Char] => cloneArray(x)
      case x: Array[Short] => cloneArray(x)
      case x: Array[Int] => cloneArray(x)
      case x: Array[Long] => cloneArray(x)
      case x: Array[Float] => cloneArray(x)
      case x: Array[Double] => cloneArray(x)
      case x: svm.Obj if x.cls.name == "java/lang/String" =>
        new String(x.members(0)("value").asInstanceOf[Array[Char]])
    }
  }.asInstanceOf[T]


  def toVirtual[T](x: Any)(implicit classes: Type.Cls => Cls, tpeObjs: Type => TpeObj): T = {
    def cloneArray  [T](x: Array[T]): Array[T] = {
      val newArray = x.clone()
      for(i <- 0 until x.length){
        newArray(i) = toVirtual[T](newArray(i))
      }
      newArray
    }
    x match{
      case null => null
      case x: Boolean => x
      case x: Byte => x
      case x: Char => x
      case x: Short => x
      case x: Int => x
      case x: Long => x
      case x: Float => x
      case x: Double => x
      case x: Array[Any] => cloneArray(x)
      case x: Array[Boolean] => cloneArray(x)
      case x: Array[Byte] => cloneArray(x)
      case x: Array[Char] => cloneArray(x)
      case x: Array[Short] => cloneArray(x)
      case x: Array[Int] => cloneArray(x)
      case x: Array[Long] => cloneArray(x)
      case x: Array[Float] => cloneArray(x)
      case x: Array[Double] => cloneArray(x)
      case x: String => svm.Obj("java/lang/String", "value" -> x.toCharArray)
    }
  }.asInstanceOf[T]
}
